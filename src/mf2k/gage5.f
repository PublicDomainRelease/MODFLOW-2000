C     Last change:  ERB   5 Jan 2001    2:19 pm
C  GAGE5 Gaging Stations
C  1/99
C
C GAGE5AL ALLOCATE SPACE FOR GAGING STATIONS
C
C     ******************************************************************
      SUBROUTINE GAGE5AL(INGAGE,ISUMIR,LSGAGE,NUMGAGE,IOUT,IUNITSFR,
     *    IUNITLAK,LKACC7,LCSTAG,LSLAKE,ICSTRM,NSTRM,NLAKES)
C
C     ******************************************************************
      IF(INGAGE.LE.0) THEN
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
      READ(INGAGE,*) NUMGAGE
      IF(NUMGAGE.LT.0) THEN
         WRITE(IOUT,1)
    1    FORMAT(1X,' NUMGAGE=0, SO GAGE IS BEING TURNED OFF')
         INGAGE=0
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
C
C     ARRAY IS SEGMENT (or LAKE) NUMBER, REACH NUMBER, UNIT#
      LSGAGE=ISUMIR
      ISUMIR=ISUMIR+NUMGAGE*3
      ISP=NUMGAGE*3
      IF (IUNITSFR.LE.0) THEN
         ICSTRM=ISUMIR
         NSTRM=1
      END IF
      IF (IUNITLAK.LE.0) THEN
         LKACC7=1
         LCSTAG=1
         LSLAKE=1
         NLAKES=1
      END IF
C
      WRITE(IOUT,101) ISP
  101 FORMAT(1X,I10,' ELEMENTS IN IX ARRAY ARE USED BY GAGE')
C
      RETURN
      END
C
C
C  GAGE5RP READ GAGING STATION INPUT FILE 
C
C     ******************************************************************
C
      SUBROUTINE GAGE5RP(IGGLST,NUMGAGE,IOUT,INGAGE)
C
C     ******************************************************************
C
C     READ GAGING STATION LOCATIONS
C     ******************************************************************
C     IGGLST ARRAY IS SEGMENT (or LAKE) NUMBER, REACH NUMBER, UNIT#
C
      DIMENSION IGGLST(3,NUMGAGE)               
C
C     ******************************************************************
C
      IF (NUMGAGE.GT.1.OR.NUMGAGE.LT.1) WRITE (IOUT,140) NUMGAGE
      IF (NUMGAGE.EQ.1) WRITE (IOUT,141) NUMGAGE
      WRITE (IOUT,150)
C READ THE FIRST RECORD OF LIST 
      DO 135 IOB=1,NUMGAGE
         READ(INGAGE,*) IGGLST(1,IOB)
         BACKSPACE INGAGE
         IF (IGGLST(1,IOB).GT.0) THEN
C           for stream:
            READ(INGAGE,*) IGGLST(1,IOB),IGGLST(2,IOB),IGGLST(3,IOB)
            WRITE(IOUT,'(4I8,5X,A40)') IOB,IGGLST(1,IOB),IGGLST(2,IOB),
     *                                 IGGLST(3,IOB)
         ELSE
            IF(IGGLST(1,IOB).EQ.0) THEN
               WRITE(IOUT,170)
               STOP
            ELSE
C              for lake:
               READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB)
               IGGLST(2,IOB)=0
               WRITE(IOUT,'(2I8,8X,I8)') IOB,IGGLST(1,IOB),
     *                                    IGGLST(3,IOB)
            END IF
         END IF
C
  135 CONTINUE
      WRITE (IOUT,180)
C
  140 FORMAT(///I4,' GAGING STATIONS WERE SPECIFIED.',/5X,'RECORDS WILL'
     1,' BE WRITTEN TO SEPARATE OUTPUT FILES REPRESENTED BY FOLLOWING UN
     2IT NUMBERS:',/5X,'(Neg. Segment No. Indicates Lake Number)'/)
  141 FORMAT(///I4,' GAGING STATION WAS SPECIFIED.',/5X,'RECORDS WILL 
     1BE WRITTEN TO SEPARATE OUTPUT FILES REPRESENTED BY FOLLOWING UNIT 
     2NUMBERS.',/5X,'(Neg. Segment No. Indicates Lake Number)'/)
  150 FORMAT(/'  GAGE #   SEGMENT   REACH   UNIT    ')
  170 FORMAT(/'*** WARNING *** Expected non-zero value for segment no.'/
     * 25X,'EXECUTION STOPPING')
 180  FORMAT(///)
      RETURN
      END       
C
C
C GAGE5I GAGING STATIONS--WRITE HEADER LINES TO OUTPUT FILES
C                       --DETERMINE & SAVE CROSS-REFERENCE INDEX
C                       --RECORD INITIAL CONDITIONS FOR LAKE GAGES
C                     
C     ******************************************************************
C
      SUBROUTINE GAGE5I(IGGLST,NUMGAGE,IOUT,IUNITGWT,STAGES,CLAKE,
     *                  NLAKES,ISTRM,NSTRM,DUM,NSOL,VOL)
C
C     ******************************************************************
C
      CHARACTER*50  LFRMAT
      DIMENSION IGGLST(3,NUMGAGE),VOL(NLAKES)               
      DIMENSION STAGES(NLAKES),CLAKE(NLAKES,NSOL),ISTRM(5,NSTRM)
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG=IGGLST(1,IOG)
         IG3=IGGLST(3,IOG)
         IF (IG.GT.0) THEN
C---Stream gage; save stream reach index; write header lines
            IG2=IGGLST(2,IOG)
            DO 20 IRCH=1,NSTRM
               IF (ISTRM(4,IRCH).EQ.IG.AND.ISTRM(5,IRCH).EQ.IG2) THEN
C---              Convert reach no. from segment list to master list
                  IGGLST(1,IOG)=IRCH
                  GO TO 30
               END IF
 20         CONTINUE
            WRITE (IOUT,100) IOG,IG3
            GO TO 10
 30         CONTINUE
            IF (IGGLST(1,IOG).GT.0) THEN
               II=IGGLST(1,IOG)
               WRITE (IG3,200) IOG,ISTRM(1,II),ISTRM(2,II),ISTRM(3,II),
     *                         ISTRM(4,II),ISTRM(5,II)
               IF (IUNITGWT.LE.0) THEN
                  WRITE (IG3,300)
               ELSE
                  IF (NSOL.EQ.1) WRITE (IG3,310)
                  IF (NSOL.GT.1) WRITE (IG3,312) NSOL
               END IF
            END IF
         ELSE
C---Lake gage; write header lines; write initial conditions
            LK=-IG
            IF (LK.GT.NLAKES) THEN
               WRITE (IOUT,105) IOG,IG3
               GO TO 10
            ELSE
               WRITE (IG3,210) IOG,LK
               IF (IUNITGWT.LE.0) THEN
                  WRITE (IG3,305)
                  WRITE (IG3,400) DUM,STAGES(LK),VOL(LK)
               ELSE
                  IF (NSOL.EQ.1) WRITE (IG3,315)
                  IF (NSOL.GT.1) WRITE (IG3,317) NSOL
                  WRITE (LFRMAT,425) NSOL
                  WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL)
               END IF
            END IF
         END IF
 10   CONTINUE
C
 100  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' NOT LOCATED ON ACTIVE',
     *   ' STREAM REACH',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
 105  FORMAT (/2X,'*** WARNING ***   GAGE ',I3,' NOT LOCATED ON ACTIVE',
     *   ' LAKE',/10X,'NO DATA WILL BE WRITTEN TO UNIT ',I3/)
 200  FORMAT (1X,'"GAGE No.',I3,':  K,I,J Coord. = ',I3,',',I3,',',I3,
     *   ';  STREAM SEGMENT = ',I3,';  REACH = ',I3,' "')
 210  FORMAT (1X,'"GAGE No.',I3,':  Lake No. = ',I3,' "')
 300  FORMAT (1X,'"DATA:  Time',7X,'Stage',7X,'Width',8X,'Flow"')
 305  FORMAT (1X,'"DATA:  Time',7X,'Stage',4X,'Volume "')
 310  FORMAT (1X,'"DATA:  Time',7X,'Stage',7X,'Width',8X,'Flow',
     *           '    Concentration"')
 312  FORMAT (1X,'"DATA:  Time',7X,'Stage',8X,'Flow    Concentration ',
     *   'of ',I3,' Solutes "')
 315  FORMAT (1X,'"DATA:  Time',7X,'Stage',4X,'Volume   Concentration"')
 317  FORMAT (1X,'"DATA:  Time',7X,'Stage',4X,'Volume   Concentration ',
     *   'of ',I3,' Solutes "')
 400  FORMAT (4X,1PE11.3,F11.3,E11.3)
 425  FORMAT ('(4X,1PE11.3,F11.3,E11.3,1X,',I3,'(E11.3,1X))')
C
      RETURN
      END
C
C
C GAGE5LO Lake GAGING STATIONS--RECORD DATA (Write output to separate files)
C                     
C     ******************************************************************
C
      SUBROUTINE GAGE5LO(IGGLST,NUMGAGE,IUNITGWT,STGNEW,CLAKE,NLAKES,
     *                  GAGETM,NSOL,VOL)
C
C     ******************************************************************
C
      CHARACTER*50  LFRMAT
      DIMENSION IGGLST(3,NUMGAGE),VOL(NLAKES)               
      DIMENSION STGNEW(NLAKES),CLAKE(NLAKES,NSOL)
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=IGGLST(3,IOG)
         IF (IG1.GT.0) THEN
            GO TO 10
         ELSE
C---Lake gage: write time, stage, volume, concentration of each solute
            LK=-IG1
            IF (LK.GT.NLAKES) THEN
               GO TO 10
            ELSE
               IF (IUNITGWT.LE.0) THEN
                  WRITE (IG3,300) GAGETM,STGNEW(LK),VOL(LK)
               ELSE
                  WRITE (LFRMAT,425) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *                             (CLAKE(LK,ISOL),ISOL=1,NSOL)
               END IF
            END IF
         END IF
 10   CONTINUE
C
 300  FORMAT (4X,1PE11.3,F11.3,E11.3)
 425  FORMAT ('(4X,1PE11.3,F11.3,E11.3,1X,',I3,'(E11.3,1X))')
C
      RETURN
      END
C
C
C GAGE5SO Stream GAGING STATIONS--RECORD DATA (Write output to separate files)
C                     
C     ******************************************************************
C
      SUBROUTINE GAGE5SO(IGGLST,NUMGAGE,IUNITGWT,STRM,
     *                  NSTRM,GAGETM,NSOL,COUT)
C
C     ******************************************************************
C
      CHARACTER*50  LFRMAT
      DIMENSION IGGLST(3,NUMGAGE)
      DIMENSION STRM(16,NSTRM),COUT(NSTRM,NSOL)
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=IGGLST(3,IOG)
         IF (IG1.GT.0) THEN
            II=IGGLST(1,IOG)
               IF (IUNITGWT.LE.0) THEN
                  WRITE (IG3,300) GAGETM,STRM(7,II),STRM(5,II),
     *                            STRM(9,II)
               ELSE
                  WRITE (LFRMAT,435) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(7,II),STRM(5,II),
     *                          STRM(9,II),(COUT(II,ISOL),ISOL=1,NSOL)
               END IF
         ELSE
            GO TO 10
         END IF
 10   CONTINUE
C
 300  FORMAT (4X,1PE11.3,1X,3(E11.3,1X))
 435  FORMAT ('(4X,1PE11.3,1X,3(E11.3,1X),',I3,'(E11.3,1X))')
C
      RETURN
      END
