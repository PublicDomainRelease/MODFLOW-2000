C     Last change:  ERB  23 Aug 2001    9:23 am
C.....   Program HYDPOST--Hydrograph post processor
C.....   This program is a post-processor for MODFLOW that reads a file
C.....   containing unformatted arrays written with module ULASAV and
C.....   writes simulation time and up to 20 head values (or other values)
C.....   to a file that can be read into a graphing program.
C.....     
C.....     HYDPOST Version 1.0        February 12, 1998
C.....      
C--------------------------------------------------------------------
      CHARACTER NAME*10,FN1*40,FN3*40,TEXT*16,ANS*1,STR10*10,FMT1*40
      CHARACTER DUMMY*1
      LOGICAL FOUND,ECHO,EXISTS,EOF,TTRAN
C--------------------------------------------------------------------
C..... Dimension array Zn at least as big as the number of model cells
C..... in one layer of the grid. Arrays JC, IR, and IJ are dimensioned
C..... for a maximum of 20 hydrograph points
C--------------------------------------------------------------------
      DIMENSION Zn(40000),JC(20),IR(20),IJ(20)
      DATA NCL,NRW/13,1/
      OPEN(UNIT=63,FILE='MESSAGE.OUT')
C----- Read file name of BAS Package input, determine number of
C----- layers, rows, and columns
      WRITE(*,*)' Enter name of BAS Package input file:'
      READ(*,'(A)') FN1
      OPEN(UNIT=79,FILE=FN1)
      READ(79,'(A1)') DUMMY
      READ(79,'(A1)') DUMMY
      READ(79,*) NLAY,NROW,NCOL
      CLOSE(79)
C----- Read name of file containing MODFLOW unformatted arrays of
C----- interest. Open file.
      WRITE(*,*)' Enter name of file with unformatted arrays',
     $ 'from MODFLOW: '
      READ(*,'(A)') FN1
CERB      OPEN(UNIT=79,FILE=FN1,FORM='UNFORMATTED')
      OPEN(UNIT=79,FILE=FN1,FORM='UNFORMATTED',ACCESS='TRANSPARENT')
C----- Read name of array from which hydrograph values will be saved
      WRITE(*,*) ' Name of unformatted array (i.e. HEAD, DRAWDOWN):'
      READ(*,'(A)') NAME
C----- Read name of file for saving hydrograph output. Open file.
      WRITE(*,*) ' Enter name of file for formatted array:'
  101 READ(*,'(A)') FN3
      INQUIRE(FILE=FN3,EXIST=EXISTS)
      IF(.NOT.EXISTS) THEN
       OPEN(UNIT=64,FILE=FN3)
      ELSE
       WRITE(*,*) ' Output file already exists. overwrite? (Y/N):'
       READ(*,'(A)') ANS
       IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
        OPEN(UNIT=64,FILE=FN3)
       ELSE
        WRITE(*,*) ' Please enter a different file name:'
        GO TO 101
       ENDIF
      ENDIF
C----- Read number and grid locations of hydrograph points.
      WRITE(*,*) ' Enter number of hydrograph points to save:'
      READ(*,*) NPTS
      WRITE(*,*) ' All points must be in the same model layer. ',
     $ ' Enter layer number: '
      READ(*,*) KLAY
      WRITE(*,10) NPTS
   10 FORMAT(' Enter',I3,' pairs of row and column locations: ')
      READ(*,*) (IR(K),JC(K),K=1,NPTS)
      DO 12 K=1,NPTS
      IJ(K)=(IR(K)-1)*NCOL+JC(K)
   12 CONTINUE
C----- Read time-transformation parameters, if option is desired.
      WRITE(*,*) ' Do you want to transform simulation time to real-'
      WRITE(*,*) ' world time using the relation:'
      WRITE(*,*) '   (real time) = (offset) + ',
     $ ' (factor)*(simulation time)         (Y/N)?'
      READ(*,'(A)') ANS
      TTRAN=.FALSE.
      IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
       TTRAN=.TRUE.
      WRITE(*,*) ' Enter offset:'
      READ(*,*) TOFFST
      WRITE(*,*) ' Enter factor:'
      READ(*,*) TFACT
      ENDIF
C----- Enter format for hydrograph output.
      WRITE(*,*) ' Enter format for output, for example, (2F10.2):'
      READ(*,'(A)') FMT1
C----- Read each unformatted array with the appropriate values. If user
C----- requests, list each unformatted array found in the file.
      WRITE(*,*) ' Do you want a screen echo of arrays in file (Y/N)?'
      READ(*,'(A)') ANS
      ECHO=.FALSE.
      IF(ANS.EQ.'Y'.OR.ANS.EQ.'y') THEN
       ECHO=.TRUE.
      ENDIF
      IF(ECHO) THEN
       WRITE(*,*) '        The following arrays were found--'
       WRITE(*,*) '        STEP     PERIOD    LAYER      NAME'
      ENDIF
       WRITE(63,*) '        The following arrays were found--'
       WRITE(63,*) '        STEP     PERIOD    LAYER      NAME'
  200 CALL ZREAD2(Zn,KLAY,NAME,KSTP,KPER,PERTIM,TOTIM,
     $           TEXT,ILAY,NCOL,NROW,FOUND,ECHO,EOF,79)
      IF(EOF) GO TO 999
      IF(.NOT.FOUND) GO TO 200
C..... Write hydrograph record for the array currently in memory.
      IF(TTRAN) THEN
       WRITE(64,FMT1) TOFFST+TFACT*TOTIM,(Zn(IJ(K)),K=1,NPTS)
      ELSE
       WRITE(64,FMT1) TOTIM,(Zn(IJ(K)),K=1,NPTS)
      ENDIF
      GO TO 200
C..... All arrays ahve been read. End Program.
  999 STOP
      END
      SUBROUTINE ZREAD2(Z,ILAYER,N10,KSTP,KPER,PERTIM,
     $ TOTIM,TEXT,ILAY,NCL,NRW,FOUND,ECHO,EOF,IUNIT)
C...... Read Z values from an unformmated file into an array dimensioned
C.....  as NCL,NRW. Arrays for which the first 10 non-blank characters
C...... do not match the text string N10 will be skipped.
C...... 
C--------------------------------------------------------------------
      CHARACTER TEXT(4)*4,N10*10,ANAME*16,NAME*20
      LOGICAL FOUND, ECHO,EOF
      DIMENSION Z(NCL,NRW)
C--------------------------------------------------------------------
      EOF=.FALSE.
      FOUND=.FALSE.
       NAME=N10//'          '
C----- Make sure first character in string is non-blank.
       CALL LEFTJ20(NAME)
C----- read an unformatted header record and concatenate name into a
C----- single string.
CERB   10  READ(IUNIT,END=8) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
   10  READ(IUNIT,ERR=8) KSTP,KPER,PERTIM,TOTIM,TEXT,NCOL,NROW,ILAY
       ANAME=TEXT(1)//TEXT(2)//TEXT(3)//TEXT(4)
C----- Set starting point in string to be first non-blank character.
      DO 11 NS=1,16
      IF(ANAME(NS:NS).NE.' ') GO TO 12
   11 CONTINUE
   12 ANAME=ANAME(NS:16)
C----- Print information on unformatted array found, if requested.
      IF(ECHO) THEN
       WRITE(*,'(3I10,1X,A)') KSTP,KPER,ILAY,ANAME
      ENDIF
C----- Write information on array found to file.
       WRITE(63,'(3I10,1X,A)') KSTP,KPER,ILAY,ANAME
C----- Read unformatted array.
       READ(IUNIT) ((Z(J,I),J=1,NCOL),I=1,NROW)
       FOUND=.FALSE.
C----- If array does not have the correct name or layer, return with
C----- flag set indicating array is not of interest.
       IF(ANAME(1:10).NE.NAME) RETURN
       IF(ILAYER.NE.ILAY) RETURN
C----- If array has the correct name and layer, return with
C----- flag set indicating array is of interest.
       FOUND=.TRUE.
      RETURN
    8 EOF=.TRUE.
      RETURN
      END
      SUBROUTINE LEFTJ20(STRING)
C----- Left-justify non-blank characters in a string of 20 characters.
C--------------------------------------------------------------------
      CHARACTER STRING*20
C--------------------------------------------------------------------
C----- Find first non-blank character. Return if string is entirely
C----- blank.
      DO 10 I=1,20
      IF(STRING(I:I).NE.' ')GO TO 20
   10 CONTINUE
      RETURN
C----- Set starting point in string to be first non-blank character.
   20 STRING=STRING(I:20)
      RETURN
      END







