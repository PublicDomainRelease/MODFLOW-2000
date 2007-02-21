! Time of File Save by ERB: 3/8/2006 3:07PM
C     revised       DEP  10 MAY 2006
C     revised       DEP  16 May 2005
C     revised:      DEP  21 Apr 2005
C     revised:      DEP  09 Feb 2004 & 22 Mar 2004
C     Last change:  ERB  15 Jan 2003    2:44 pm
C     REVISED:      LFK  07 Aug 2003
C  GAGE5 Gaging Stations
C  1/99
C
C-------SUBROUTINE GWF1GAG5DF
C
Cdep  Added new variables for Unsaturated-Zone Flow Beneath Streams
      SUBROUTINE GWF1GAG5DF(NUMGAGE,LSGAGE,NSTRM,ICSTRM,NLAKES,LKACC7,
     &                      LCSTAG,LSLAKE,NLAKESAR,NSTRMAR,NSS,NSSAR,
     &                      LCIVAR,NUZST,NUMAVE)
C     VERSION  5:CONNECTED TO LAK3 PACKAGE AND MODFLOW-GWT-- May 10,2006
C     ******************************************************************
C     INITIALIZE POINTER VARIABLES USED BY SFR2 TO SUPPORT LAKE3 AND
C     GAGE PACKAGES AND THE GWT PROCESS revised dep for Unsaturated flow 
C     ******************************************************************
      NUMGAGE=0
      LSGAGE=1
C
C     CONNECTION TO SFR2 PACKAGE
Cdep   added Unsaturated-Zone Flow variables for SFR2
        NSTRM=0
        NSTRMAR=1
        NSS=0
        NSSAR=1
        ICSTRM=1
        LCIVAR=1
        NUZST=1
        NUMAVE=1
C
C     CONNECTION TO LAK3 PACKAGE
        NLAKES=0
        NLAKESAR=1
        LKACC7=1
        LCSTAG=1
        LSLAKE=1
      RETURN
      END

C GWF1GAG5ALP ALLOCATE SPACE FOR GAGING STATIONS
C
C     ******************************************************************
      SUBROUTINE GWF1GAG5ALP(INGAGE,ISUMIR,ISUMRX,LSGAGE,NUMGAGE,IOUT,
     *    IUNITSFR,IUNITLAK,LKACC7,LCSTAG,LSLAKE,ICSTRM,LCIVAR)
C
C     ******************************************************************
      IF(IUNITSFR.LE.0.AND.IUNITLAK.LE.0) THEN
         WRITE(IOUT,1)
    1    FORMAT(1X,' GAGE PACKAGE ACTIVE EVEN THOUGH SFR AND LAK3 ',
     &         'PACKAGES ARE INACTIVE: ',
     &         'GAGE PACKAGE IS BEING TURNED OFF')
         INGAGE=0
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
      READ(INGAGE,*) NUMGAGE
      IF(NUMGAGE.LE.0) THEN
         WRITE(IOUT,2)
    2    FORMAT(1X,' NUMGAGE=0, SO GAGE IS BEING TURNED OFF')
         INGAGE=0
         LSGAGE=1
         NUMGAGE=0
         RETURN
      END IF
C
C     ARRAY IS SEGMENT (or LAKE) NUMBER, REACH NUMBER, UNIT#.
      LSGAGE=ISUMIR
      ISUMIR=ISUMIR+NUMGAGE*4
      ISP=NUMGAGE*4
C
C     DEFINE ICSTRM AND NSTRM IF SFR2 NOT ACTIVE.
      IF (IUNITSFR.LE.0) THEN
         ICSTRM=ISUMIR
         ISUMIR=ISUMIR+5   ! ERB
         LCIVAR=ISUMIR
         ISUMIR=ISUMIR+2   ! ERB
         ISP=ISP+7
      END IF
C
C     DEFINE LKACC7,LCSTAG,LSLAKE IF LAK3 NOT ACTIVE.
      ISPRX=0
      IF (IUNITLAK.LE.0) THEN
         LKACC7=ISUMRX
         ISUMRX=ISUMRX+1
         LCSTAG=ISUMRX
         ISUMRX=ISUMRX+1
         LSLAKE=ISUMRX
         ISUMRX=ISUMRX+1
         ISPRX=ISPRX+3
      END IF
C
C     PRINT SPACE USED BY GAGE PACKAGE.
      WRITE (IOUT,101)ISPRX
  101 FORMAT(1X,I10,' ELEMENTS IN RX ARRAY ARE USED BY GAGE')
      WRITE(IOUT,102) ISP
  102 FORMAT(1X,I10,' ELEMENTS IN IR ARRAY ARE USED BY GAGE')
C
      RETURN
      END
C
C
C  GWF1GAG5RPP READ GAGING STATION INPUT FILE
C
C     ******************************************************************
C
      SUBROUTINE GWF1GAG5RPP(IGGLST,NUMGAGE,IOUT,INGAGE)
C
C     ******************************************************************
C
C     READ GAGING STATION LOCATIONS
C     ******************************************************************
C     IGGLST ARRAY IS (1) SEGMENT (or LAKE) NUMBER; (2) REACH NUMBER
C          (null for LAKE); (3) UNIT #; and (4) OUTTYPE
C
      DIMENSION IGGLST(4,NUMGAGE)
C
C     ******************************************************************
C
      IF (NUMGAGE.GT.1.OR.NUMGAGE.LT.1) WRITE (IOUT,140) NUMGAGE
      IF (NUMGAGE.EQ.1) WRITE (IOUT,141) NUMGAGE
C INITIALIZE GAGE COUNTERS
         NSG=0
         NLG=0
C READ THE FIRST RECORD OF LIST
      DO 135 IOB=1,NUMGAGE
         READ(INGAGE,*) IGGLST(1,IOB)
         BACKSPACE INGAGE
         IF (IGGLST(1,IOB).GT.0) THEN
C           for stream:
            NSG=NSG+1
            READ(INGAGE,*) IGGLST(1,IOB),IGGLST(2,IOB),IGGLST(3,IOB),
     *                     IGGLST(4,IOB)
         ELSE
            IF(IGGLST(1,IOB).EQ.0) THEN
               WRITE(IOUT,170)
               CALL USTOP(' ')
            ELSE
C              for lake:
               NLG=NLG+1
               READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB)
               IGGLST(2,IOB)=0
C              check for negative unit number, which designates OUTTYPE is read
               IF (IGGLST(3,IOB).LT.0) THEN
                BACKSPACE INGAGE
                READ(INGAGE,*) IGGLST(1,IOB),IGGLST(3,IOB),IGGLST(4,IOB)
               ELSE
                 IGGLST(4,IOB)=0
               END IF
            END IF
         END IF
C
  135 CONTINUE
C
C PRINT STREAM GAGES
      IF (NSG.GT.0) THEN
        WRITE (IOUT,*) 'Stream Gages:'
        WRITE (IOUT,150)
        DO 136 IOB=1,NUMGAGE
          IF (IGGLST(1,IOB).GT.0) THEN
            WRITE(IOUT,'(5I8,13X,A40)') IOB,IGGLST(1,IOB),
     *                        IGGLST(2,IOB),IGGLST(3,IOB),IGGLST(4,IOB)
          END IF
  136   CONTINUE
      END IF
C
C PRINT LAKE GAGES
      IF (NLG.GT.0) THEN
        WRITE (IOUT,*) 'Lake Gages:'
        WRITE (IOUT,155)
        DO 137 IOB=1,NUMGAGE
          IF (IGGLST(1,IOB).LT.0) THEN
            IF (IGGLST(3,IOB).LT.0) THEN
              WRITE(IOUT,'(4I8)') IOB,IGGLST(1,IOB),
     *                  IGGLST(3,IOB),IGGLST(4,IOB)
            ELSE
              WRITE(IOUT,'(3I8)') IOB,IGGLST(1,IOB),IGGLST(3,IOB)
            END IF
          END IF
  137   CONTINUE
      END IF
      WRITE (IOUT,180)
C
  140 FORMAT(///I4,' GAGING STATIONS WERE SPECIFIED.',/5X,'(Lakes are ',
     *'identified by a negative value of the Lake Number)',/5X,'RECORDS'
     1,' WILL BE WRITTEN TO SEPARATE OUTPUT FILES REPRESENTED BY ',
     2'FOLLOWING UNIT NUMBERS:',/)
  141 FORMAT(///I4,' GAGING STATION WAS SPECIFIED.',/5X,'(Lakes are ',
     *'identified by a negative value of the Lake Number)',/5X,'RECORDS'
     1,' WILL BE WRITTEN TO SEPARATE OUTPUT FILE REPRESENTED BY ',
     2'FOLLOWING UNIT NUMBER:')
  150 FORMAT('  GAGE #   SEGMENT   REACH   UNIT   OUTTYPE')
  155 FORMAT('  GAGE #    LAKE     UNIT   OUTTYPE')
  170 FORMAT(/'*** ERROR *** Expected non-zero value for segment no.'/
     * 25X,'EXECUTION STOPPING')
 180  FORMAT(///)
      RETURN
      END
C
C
C GWF1GAG5I GAGING STATIONS--WRITE HEADER LINES TO OUTPUT FILES
C                       --DETERMINE & SAVE CROSS-REFERENCE INDEX
C                       --RECORD INITIAL CONDITIONS FOR LAKE GAGES
Cdep  revised to include unsaturated flow in SFR2 may 10, 2006
C     ******************************************************************
C
      SUBROUTINE GWF1GAG5I(IGGLST,NUMGAGE,IOUT,IUNITGWT,STAGES,CLAKE,
     *                  NLAKES,ISTRM,NSTRM,IDIVAR,DUM,NSOL,VOL,
     *                  NLAKESAR,NSTRMAR,NSSAR)
C
C     ******************************************************************
C
      CHARACTER*1 A
      CHARACTER*2 B
      CHARACTER*7 CONCNAME
      CHARACTER*9 DCTSNAME
      CHARACTER*10 DCCMNAME
      CHARACTER*1256  LFRMAT
C TEMPORARY ARRAYS
      ALLOCATABLE CONCNAME(:),DCTSNAME(:),DCCMNAME(:),DUMMY(:,:)
      DIMENSION IGGLST(4,NUMGAGE),VOL(NLAKESAR)
      DIMENSION STAGES(NLAKESAR),CLAKE(NLAKESAR,NSOL),ISTRM(5,NSTRMAR)
      DIMENSION IDIVAR(2,NSSAR)
C ALLOCATE TEMPORARY ARRAYS
      ALLOCATE(CONCNAME(NSOL),DCTSNAME(NSOL),DCCMNAME(NSOL),
     *DUMMY(NLAKESAR,NSOL))
C
C     ******************************************************************
C
      do i = 1,NLAKESAR
         DUMMY(i,1)=0.0
      end do

C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG.GT.0) THEN
C---Stream gage; save stream reach index; write header lines
            IG2=IGGLST(2,IOG)
            DO 20 IRCH=1,NSTRM
               IF (ISTRM(4,IRCH).EQ.IG.AND.ISTRM(5,IRCH).EQ.IG2) THEN
C---              Convert reach no. from segment list to master list
                  IGGLST(2,IOG)=IRCH
                  GO TO 30
               END IF
 20         CONTINUE
            WRITE (IOUT,100) IOG,IG3
            GO TO 10
 30         CONTINUE
            IF (IGGLST(2,IOG).GT.0) THEN
               II=IGGLST(2,IOG)
               WRITE (IG3,200) IOG,ISTRM(1,II),ISTRM(2,II),ISTRM(3,II),
     *                         ISTRM(4,II),ISTRM(5,II)
C---Check if gage station is for a diversion (outtype is 5)
               IF(IGGLST(4,IOG).EQ.5) THEN
                 IF(IDIVAR(1,IG).LE.0.OR.IDIVAR(2,IG).GT.0) THEN
                   WRITE(IG3,201) IOG,IG
                   IGGLST(4,IOG)=0
                 ELSE IF (ISTRM(5,II).NE.1) THEN
                   WRITE(IG3,202) IOG,IG,ISTRM(5,II)
                   IGGLST(4,IOG)=0
                 ELSE
                   WRITE(IG3,203) IG,IDIVAR(1,IG),IDIVAR(2,IG)
                 END IF
               END IF
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
Cdep---Added new options for printing unsaturated flow beneath streams
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,250)
                   CASE (1)
                     WRITE (IG3,255)
                   CASE (2)
                     WRITE (IG3,260)
                   CASE (3)
                     WRITE (IG3,250)
                   CASE (4)
                     WRITE (IG3,265)
                   CASE (5)
                     WRITE (IG3,267)
                   CASE (6)
                     WRITE (IG3,268)
                   CASE (7)
                     WRITE (IG3,269)
                 END SELECT
               ELSE
C              TRANSPORT ON
C                GET OUTTYPE
                 IF (NSOL.LE.0) THEN
                    WRITE (IOUT,240)
                     CALL USTOP(' ')
                 END IF
                 SELECT CASE (IGGLST(4,IOG))
                 CASE(0)
                  IF (NSOL.EQ.1) WRITE (IG3,270)
                  IF (NSOL.GT.1) WRITE (IG3,272) NSOL
                 CASE(1)
                  IF (NSOL.EQ.1) WRITE (IG3,275)
                  IF (NSOL.GT.1) WRITE (IG3,277) NSOL
                 CASE(2)
                  IF (NSOL.EQ.1) WRITE (IG3,280)
                  IF (NSOL.GT.1) WRITE (IG3,282) NSOL
                 CASE(3)
                  IF (NSOL.EQ.1) WRITE (IG3,281)
                  IF (NSOL.GT.1) WRITE (IG3,284) NSOL
                 CASE(4)
                  IF (NSOL.EQ.1) WRITE (IG3,285)
                  IF (NSOL.GT.1) WRITE (IG3,287) NSOL
                 CASE(5)
                  IF (NSOL.EQ.1) WRITE (IG3,290)
                  IF (NSOL.GT.1) WRITE (IG3,292) NSOL
C LFK:  warning messages added below
                 CASE(6)
                  WRITE (IG3,294)
                 CASE(7)
                  WRITE (IG3,294)
                 END SELECT
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
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,305)
                     WRITE (IG3,400) DUM,STAGES(LK),VOL(LK)
                   CASE (1)
                     WRITE (IG3,306)
                     WRITE (IG3,401) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *DUM,DUM,DUM,DUM,DUM,DUM,DUM
                   CASE (2)
                     WRITE (IG3,307)
                     WRITE (IG3,402) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *DUM
                   CASE (3)
                     WRITE (IG3,308)
                     WRITE (IG3,403) DUM,STAGES(LK),VOL(LK),DUM,DUM,DUM,
     *DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM
                 END SELECT
               ELSE
C              TRANSPORT ON
C                Prepare array of header names for multiple constituents
                 DFLAG=0
                 IF(IGGLST(4,IOG).EQ.2.OR.IGGLST(4,IOG).EQ.3) DFLAG=1
                 DO 1000 ISOL=1,NSOL
                   IF (ISOL.LT.10) THEN
                     WRITE(A,'(I1)') ISOL
                     CONCNAME(ISOL)='Conc'//'_0'//A
                     IF(DFLAG.EQ.1) THEN
                       DCTSNAME(ISOL)='D-C'//'_0'//A//'-TS'
                       DCCMNAME(ISOL)='D-C'//'_0'//A//'-Cum'
                     END IF
                   ELSE IF (ISOL.GT.9.AND.ISOL.LT.100) THEN
                     WRITE(B,'(I2)') ISOL
                     CONCNAME(ISOL)='Conc'//'_'//B
                     IF(DFLAG.EQ.1) THEN
                       DCTSNAME(ISOL)='D-C'//'_'//B//'-TS'
                       DCCMNAME(ISOL)='D-C'//'_'//B//'-Cum'
                     END IF
                   ELSE
                     WRITE(IOUT,*) '***ERROR***  NSOL TOO BIG'
                     CALL USTOP(' ')
                   END IF
 1000            CONTINUE
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                 CASE(0)
                   WRITE (LFRMAT,315) NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,425) NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL)
                 CASE(1)
                   WRITE (LFRMAT,316) NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,426) NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM
                 CASE(2)
                   WRITE (LFRMAT,317) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL),
     * (DCTSNAME(ISOL),ISOL=1,NSOL),(DCCMNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,427) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL)
                 CASE(3)
                   WRITE (LFRMAT,318) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) (CONCNAME(ISOL),ISOL=1,NSOL),
     * (DCTSNAME(ISOL),ISOL=1,NSOL),(DCCMNAME(ISOL),ISOL=1,NSOL)
                   WRITE (LFRMAT,428) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) DUM,STAGES(LK),VOL(LK),
     *              (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,DUM,
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL),
     *              DUM,DUM,(DUMMY(LK,ISOL),ISOL=1,NSOL)
                 END SELECT
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
 201  FORMAT (/2X,'*** WARNING ***  GAGE ',I3,' ON STREAM SEGMENT ',I3,
     *   ' NOT A DIVERSION AS THERE IS NO UPSTREAM SEGMENT OR ',
     *   ' DIVERSION TYPE (IPRIOR)',/10X,
     *   ' RESETTING OUTTYPE FROM 5 TO 0')
 202  FORMAT (/2X,'*** WARNING ***  GAGE ',I3,' ON STREAM SEGMENT ',I3,
     *   ' REACH NO. ',I3,' IS NOT LOCATED ON FIRST REACH OF A',
     *   ' DIVERSION',/10X,' RESETTING OUTTYPE FROM 5 TO 0')
 203  FORMAT (/2X,'STREAM SEGMENT ',I3,' IS DIVERTED FROM SEGMENT ',I3,
     *        ' DIVERSION TYPE IS IPRIOR OF ',I3)
 210  FORMAT (1X,'"GAGE No.',I3,':  Lake No. = ',I3,' "')
 240  FORMAT (/2X,'*** ERROR ***   NSOL NEEDED BUT NOT DEFINED IN ',
     *   'GAGE PACKAGE.  PROGRAM TERMINATING.')
C     minor format adjustments below by LFK, July 2006
C 250  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow"')
 250  FORMAT (1X,'"DATA:   Time',7X,'Stage',8X,'Flow"')
C 255  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 255  FORMAT (1X,'"DATA:   Time',7X,'Stage',8X,'Flow',
     *           8X,'Depth',8X,'Width',7X,'M-P Flow"')
C 260  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 260  FORMAT (1X,'"DATA:   Time',7X,'Stage',8X,'Flow',
     *           8X,'Cond.',7X,'HeadDiff',5X,'Hyd.Grad."')
C 265  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 265  FORMAT (1X,'"DATA:   Time',8X,'Stage',8X,'Flow',
     *           9X,'Depth',8X,'Width',8X,'M-P Flow',
     *           6X,'Cond.',6X,'HeadDiff',5X,'Hyd.Grad."')
C 267  FORMAT (1X,'" DATA:   Time',8X,'Stage',5X,
 267  FORMAT (1X,'"DATA:   Time',7X,'Stage',6X,
     *           'Max. Rate',7X,'Rate Diverted',3X,
     *           'Upstream Flow"')
Cdep---added option for printing unsaturated flow beneath streams
C 268  FORMAT (1X,'" DATA:   Time',6X,'Stage',8X,'Depth',7X,
 268  FORMAT (1X,'"DATA:   Time',6X,'Stage',8X,'Depth',7X,
     *           'GW Head',6X,'M-P Flow',3X,'Stream Loss',4X,
     *           'GW Rech.',5X,'Chnge UZ Stor.',3X,
     *           'Vol. UZ Stor."')
Cdep---added option for printing water content in unsaturated zone
 269  FORMAT (1X,'"DATA:  Time',8X,'Depth',6X,
     *           'Width Ave. Water Content',4X,
     *           'Cell 1 Water Content"')
C     following formats modified by LFK, July 2006:
 270  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           '    Concentration"')
 272  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           '     Concentration ',
     *           'of ',I3,' Solutes "')
C275  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 275  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           8X,'Depth',7X,'Width',5X,'M-P Flow',
     *           '  Concentration"')
 277  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           8X,'Depth',7X,'Width',5X,'M-P Flow',
     *           '  Concentration ',
     *           'of ',I3,' Solutes "')
C280  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 280  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           8X,'Cond.',5X,'HeadDiff',4X,'Hyd.Grad.',
     *           '  Concentration"')
 281  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           '    Concentration    Load"')
 282  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           8X,'Cond.',5X,'HeadDiff',4X,'Hyd.Grad.',
     *           '  Concentration ',
     *           'of ',I3,' Solutes "')
 284  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           '    Concentration  & Load ',
     *           'of ',I3,' Solutes "')
C285  FORMAT (1X,'" DATA:   Time',8X,'Stage',9X,'Flow',
 285  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           8X,'Depth',7X,'Width',5X,'M-P Flow',
     *           6X,'Cond.',5X,'HeadDiff',3X,'Hyd.Grad.',
     *           '  Concentration   Load"')
 287  FORMAT (1X,'"DATA:   Time',6X,'Stage',7X,'Flow',
     *           8X,'Depth',7X,'Width',5X,'M-P Flow',
     *           6X,'Cond.',5X,'HeadDiff',3X,'Hyd.Grad.',
     *           '   Concentration  &  Load ',
     *           'of ',I3,' Solutes "')
C290  FORMAT (1X,'" DATA:   Time',5X,'Stage',5X,
 290  FORMAT (1X,'"DATA:   Time',6X,'Stage',5X,
     *           'Max. Rate',2X,'Rate Diverted',1X,
     *           'Upstream Flow Concentration',3X,
     *           'Load"')
 292  FORMAT (1X,'"DATA:   Time',6X,'Stage',5X,
     *           'Max. Rate',2X,'Rate Diverted',1X,
     *           'Upstream Flow Concentration & ',
     *           'Load of ',I3,' Solutes "')
C  LFK
 294  FORMAT (1X,'"Warning:  Unsaturated flow options ',
     *           'not compatible with GWT"')
C305  FORMAT (1X,'" DATA:   Time',7X,'Stage(H)',5X,'Volume "')
 305  FORMAT (1X,'"DATA:   Time',6X,'Stage(H)',5X,'Volume "')
c markstro
c 306  FORMAT (1X,'"DATA:  Time',6X,'Stage(H)',2X,'Volume',5X,'Precip.',
c     1 5x,'Evap.',5x,'Runoff',4x,'GW-Inflw',3x,'GW-Outflw',2x,'SW-Inflw'
c     2 ,3x,'SW-Outflw',x,'Withdrawal',1x,'Lake-Inflx',x,'Total-Cond "')
C306  FORMAT (2X,'"DATA:   Time',7X,'Stage(H)',3X,'Volume',6X,'Precip.',
 306  FORMAT (1X,'"DATA:   Time',6X,'Stage(H)',4X,'Volume',6X,'Precip.',
     1 6x,'Evap.',6x,'Runoff',5x,'GW-Inflw',4x,'GW-Outflw',3x,'SW-Inflw'
     2 ,4x,'SW-Outflw',2x,'Withdrawal',2x,'Lake-Inflx',2x,'Total-Cond "'
     &   )
c end markstro
 307  FORMAT (1X,'"DATA:   Time',6X,'Stage(H)',4X,'Volume',
C     * 6x,'Del-H-TS',4x,'Del-V-TS',3x,'Del-H-Cum',3x,'Del-V-Cum "')
     * 6x,'Del-H-TS',4x,'Del-V-TS',3x,'Del-H-Cum',3x,'Del-V-Cum "')
c markstro
c 308  FORMAT (1X,'"DATA:  Time',6X,'Stage(H)',2X,'Volume',5X,'Precip.',
c     1 5x,'Evap.',5x,'Runoff',4x,'GW-Inflw',3x,'GW-Outflw',2x,'SW-Inflw'
c     2 ,3x,'SW-Outflw',x,'Withdrawal',1x,'Lake-Inflx',x,'Total-Cond ',
c     * 2x,'Del-H-TS',3x,'Del-V-TS',2x,'Del-H-Cum',2x,'Del-V-Cum "')
C308  FORMAT (2X,'"DATA:   Time',7X,'Stage(H)',3X,'Volume',6X,'Precip.',
 308  FORMAT (1X,'"DATA:   Time',6X,'Stage(H)',4X,'Volume',6X,'Precip.',
C     1 6x,'Evap.',6x,'Runoff',5x,'GW-Inflw',4x,'GW-Outflw',3x,'SW-Inflw'
     1 6x,'Evap.',7x,'Runoff',4x,'GW-Inflw',4x,'GW-Outflw',3x,'SW-Inflw'
     2 ,4x,'SW-Outflw',2x,'Withdrawal',2x,'Lake-Inflx',2x,'Total-Cond ',
     * 3x,'Del-H-TS',4x,'Del-V-TS',3x,'Del-H-Cum',3x,'Del-V-Cum "')
c end markstro
C 315  FORMAT ('( 2X,''"DATA:   Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
 315  FORMAT ('( 1X,''"DATA:   Time'',6X,''Stage(H)'',4X,''Volume'',2X,'
     *,I2,'A12, '' "'')')
C 316  FORMAT ('( 2X,''"DATA:  Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
 316  FORMAT ('( 1X,''"DATA:  Time'',7X,''Stage(H)'',4X,''Volume'',2X,'
     *,I2,'A12,6X,''Precip'',7x,''Evap.'',6x,''Runoff'',5x,''GW-Inflw'',
     *4x,''GW-Outflw'',3x,''SW-Inflw'',4x,''SW-Outflw'',2x,''Withdrawal'
     *',2x,''Lake-Inflx'',2x,''Total-Cond "'')')
C 317  FORMAT ('( 2X,''"DATA:   Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
 317  FORMAT ('( 1X,''"DATA:   Time'',6X,''Stage(H)'',4X,''Volume'',2X,'
c     *,I2,'A12,4x,''Del-H-TS'',4x,''Del-V-TS'',2x,',I2,'A12,3x,
     *,I2,'A12,5x,''Del-H-TS'',5x,''Del-V-TS  '', ',I2,'A12,4x,
c     *''Del-H-Cum'',3x,''Del-V-Cum'',0x,',I2,'A12,'' "'')')
     *''Del-H-Cum'',4x,''Del-V-Cum '', ',I2,'A12,'' "'')')
C318  FORMAT ('( 2X,''"DATA:   Time'',7X,''Stage(H)'',3X,''Volume'',3X,'
 318  FORMAT ('( 1X,''"DATA:   Time'',6X,''Stage(H)'',4X,''Volume'',2X,'
C     *,I2,'A12,7X,''Precip'',5x,''Evap.'',5x,''Runoff'',4x,''GW-Inflw'',
     *,I2,'A12,6X,''Precip'',7x,''Evap.'',7x,''Runoff'',5x,''GW-Inflw'',
     *3x,''GW-Outflw'',3x,''SW-Inflw'',4x,''SW-Outflw'',2x,''Withdrawal'
     *',2x,''Lake-Inflx'',2x,''Total-Cond'',4x,''Del-H-TS'',5x,
c     *,0x,',I2,'A12,3x,
     *''Del-V-TS '', ',I2,'A12,4x,
c     *''Del-H-Cum'',3x,''Del-V-Cum'',0x,',I2,'A12,'' "'')')
     *''Del-H-Cum'',4x,''Del-V-Cum '', ',I2,'A12,'' "'')')
cdep  Made initial lake output for format statement 400 consistent with
cdep   subsequent output for transient simulations. June 17, 2006
C 400  FORMAT (4X,1PE11.3,0PF11.3,1PE11.3)
 400  FORMAT (4X,1PE12.5,0PF12.5,1X,1PE12.5)
 401  FORMAT (4X,1PE12.5,0PF12.5,1X,1P11E12.5)
 402  FORMAT (4X,1PE12.5,0PF12.5,1X,1P5E12.5)
 403  FORMAT (4X,1PE12.5,0PF12.5,1X,1P15E12.5)
 425  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X))')
 426  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
C     *10E11.3)')
     *10E12.5)')
 427  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
     *E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
     *',I3,'(E12.5,1X))')
 428  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
     *10E12.5,E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
     *',I3,'(E12.5,1X))')
C
C  RELEASE MEMORY
      DEALLOCATE(CONCNAME,DCTSNAME,DCCMNAME,DUMMY)
      RETURN
      END
C
C
C SGWF1GAG5LO Lake GAGING STATIONS--RECORD DATA (Write output to separate files)
C
C     ******************************************************************
C
      SUBROUTINE SGWF1GAG5LO(IGGLST,NUMGAGE,IUNITGWT,STGNEW,CLAKE,
     *                  NLAKES,GAGETM,NSOL,VOL,
     *                  PRECIP,EVAP,RNF,
     *                  GWIN,GWOUT,SURFIN,SURFOT,
     *                  WTHDRW,FLXINL,SUMCNN,
     *                  STGOLD2,VOLOLD,STAGES,VOLINIT,CLKOLD,CLAKINIT)
C
C     ******************************************************************
C
Cdep  made PRECIP and EVAP arrays double precision--2/14/2007
      CHARACTER*1256  LFRMAT
      DIMENSION IGGLST(4,NUMGAGE),VOL(NLAKES)
      DIMENSION STGNEW(NLAKES),CLAKE(NLAKES,NSOL)
      DIMENSION RNF(NLAKES),
     * GWIN(NLAKES),GWOUT(NLAKES),SURFIN(NLAKES),SURFOT(NLAKES),
     * WTHDRW(NLAKES),FLXINL(NLAKES),SUMCNN(NLAKES),
     * STGOLD2(NLAKES),VOLOLD(NLAKES),STAGES(NLAKES),VOLINIT(NLAKES),
     * CLKOLD(NLAKES,NSOL),CLAKINIT(NLAKES,NSOL)
      DOUBLE PRECISION PRECIP(NLAKES),EVAP(NLAKES)
      ALLOCATABLE DELCTS(:,:),DELCCUM(:,:)
      ALLOCATE(DELCTS(NLAKES,NSOL),DELCCUM(NLAKES,NSOL))
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=ABS(IGGLST(3,IOG))
         IF (IG1.GT.0) THEN
            GO TO 10
         ELSE
C---Lake gage: write time, stage, volume, concentration of each solute
            LK=-IG1
            IF (LK.GT.NLAKES) THEN
               GO TO 10
            ELSE
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                 CASE (0)
                   WRITE (IG3,300) GAGETM,STGNEW(LK),VOL(LK)
                 CASE (1)
                   WRITE (IG3,401) GAGETM,STGNEW(LK),VOL(LK),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK)
                 CASE (2)
                   WRITE (IG3,402) GAGETM,STGNEW(LK),VOL(LK),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK)
                 CASE (3)
                   WRITE (IG3,403) GAGETM,STGNEW(LK),VOL(LK),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK)
                 END SELECT
               ELSE
C              TRANSPORT ON
                 SELECT CASE (IGGLST(4,IOG))
                 CASE (0)
                   WRITE (LFRMAT,425) NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *                             (CLAKE(LK,ISOL),ISOL=1,NSOL)
                 CASE (1)
                   WRITE (LFRMAT,426) NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *       (CLAKE(LK,ISOL),ISOL=1,NSOL),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK)
                 CASE (2)
                   DO 744 ISOL=1,NSOL
                     DELCTS(LK,ISOL)=CLAKE(LK,ISOL)-CLKOLD(LK,ISOL)
                     DELCCUM(LK,ISOL)=CLAKE(LK,ISOL)-CLAKINIT(LK,ISOL)
  744              CONTINUE
                   WRITE (LFRMAT,427) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *       (CLAKE(LK,ISOL),ISOL=1,NSOL),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       (DELCTS(LK,ISOL),ISOL=1,NSOL),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK),
     *       (DELCCUM(LK,ISOL),ISOL=1,NSOL)
                 CASE (3)
                   DO 745 ISOL=1,NSOL
                     DELCTS(LK,ISOL)=CLAKE(LK,ISOL)-CLKOLD(LK,ISOL)
                     DELCCUM(LK,ISOL)=CLAKE(LK,ISOL)-CLAKINIT(LK,ISOL)
  745              CONTINUE
                   WRITE (LFRMAT,428) NSOL,NSOL,NSOL
                   WRITE (IG3,LFRMAT) GAGETM,STGNEW(LK),VOL(LK),
     *       (CLAKE(LK,ISOL),ISOL=1,NSOL),PRECIP(LK),
     *       EVAP(LK),RNF(LK),GWIN(LK),GWOUT(LK),SURFIN(LK),SURFOT(LK),
     *       WTHDRW(LK),FLXINL(LK),SUMCNN(LK),
     *       STGNEW(LK)-STGOLD2(LK),VOL(LK)-VOLOLD(LK),
     *       (DELCTS(LK,ISOL),ISOL=1,NSOL),
     *       STGNEW(LK)-STAGES(LK),VOL(LK)-VOLINIT(LK),
     *       (DELCCUM(LK,ISOL),ISOL=1,NSOL)
                 END SELECT
               END IF
            END IF
         END IF
 10   CONTINUE
C
Clfk  change formats in following for consistency with p/o for initial conds.
 300  FORMAT (4X,1PE12.5,0PF12.5,1X,1PE12.5)
 401  FORMAT (4X,1PE12.5,0PF12.5,1X,1P11E12.5)
 402  FORMAT (4X,1PE12.5,0PF12.5,1X,1P5E12.5)
 403  FORMAT (4X,1PE12.5,0PF12.5,1X,1P15E12.5)
 425  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X))')
 426  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
     *10E12.5)')
 427  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
     *E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
     *',I3,'(E12.5,1X))')
 428  FORMAT ('(4X,1PE12.5,0PF12.5,1X,1PE12.5,1X,',I3,'(E12.5,1X),
     *10E12.5,E12.5,X,E12.5,X,',I3,'(E12.5,1X),E12.5,X,E12.5,X,
     *',I3,'(E12.5,1X))')
C
C  RELEASE MEMORY
      DEALLOCATE(DELCTS,DELCCUM)
      RETURN
      END
C
C
C SGWF1GAG5SO Stream GAGING STATIONS--RECORD DATA (Write output to separate files)
C
Cdep---file revised to include unsaturated flow beneath streams
C     ******************************************************************
C
      SUBROUTINE SGWF1GAG5SO(IGGLST,NUMGAGE,IUNITGWT,STRM,ISEG,
     *                  NSEGDIM,NSTRM,GAGETM,NSOL,COUT,SFRQ,NSS,SEG,
     *                  SGOTFLW,IDIVAR,AVWAT,WAT1,AVDPT,NUZST,NUMAVE,
     *                  IBD )
C
C     ******************************************************************
C
      CHARACTER*50  LFRMAT
      REAL SFRQ(5,NSTRM)
      ALLOCATABLE CLOAD(:)
Cdep   Increased dimensions for SEG arrays dep 4/21/2005
      DIMENSION IGGLST(4,NUMGAGE),ISEG(4,NSEGDIM)
      DIMENSION STRM(24,NSTRM),COUT(NSTRM,NSOL)
      DIMENSION SEG(26,NSS),IDIVAR(2,NSS),SGOTFLW(NSS)
      DIMENSION AVWAT(NUZST,NUMAVE),AVDPT(NUZST,NUMAVE)
      DIMENSION WAT1(NUZST,NUMAVE)
C     ALLOCATE MEMORY
      ALLOCATE(CLOAD(NSOL))
C
C     ******************************************************************
C
C  LOOP OVER GAGING STATIONS
      DO 10 IOG=1,NUMGAGE
         IG1=IGGLST(1,IOG)
         IG3=IGGLST(3,IOG)
         IF (IG1.GT.0) THEN
            II=IGGLST(2,IOG)
C
C        CALCULATE STREAM DEPTH
              DEPTH=STRM(7,II)
                IF (ISEG(1,IG1).EQ.0) THEN
                 DEPTH=STRM(15,II)-STRM(3,II)
              END IF
C       COMPUTE DIVERSION RATES IF OUTTYPE IS 5
              IF(IGGLST(4,IOG).EQ.5) THEN
                   IUPSEG=IDIVAR(1,IG1)
                   UPSTRFLW=STRM(10,II)+SGOTFLW(IUPSEG)
                   IF(IDIVAR(2,IG1).GE.-1) PMXDVRT=SEG(2,IG1)
                   IF(IDIVAR(2,IG1).EQ.-2) PMXDVRT=SEG(2,IG1)*UPSTRFLW
                   IF(IDIVAR(2,IG1).EQ.-3) PMXDVRT=UPSTRFLW-SEG(2,IG1)
              END IF
C
C              TRANSPORT OFF
               IF (IUNITGWT.LE.0) THEN
C                GET OUTTYPE
                 SELECT CASE (IGGLST(4,IOG))
                   CASE (0)
                     WRITE (IG3,250) GAGETM,STRM(15,II),STRM(9,II)
                   CASE (1)
                     WRITE (IG3,255) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II)
                   CASE (2)
                     WRITE (IG3,260) GAGETM,STRM(15,II),STRM(9,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II)
                   CASE (3)
                     WRITE (IG3,250) GAGETM,STRM(15,II),STRM(9,II)
                   CASE (4)
                     WRITE (IG3,265) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II)
C          OUTTYPE 5 IS USED TO PRINT TIME SERIES FOR DIVERSIONS.
                   CASE (5)
                     WRITE (IG3,270) GAGETM,STRM(15,II),PMXDVRT,
     *                    STRM(10,II),UPSTRFLW
Cdep       OUTTYPE 6 IS USED TO PRINT TIME SERIES FOR UNSATURATED FLOW.
                   CASE (6)
                     WRITE (IG3,275) GAGETM,STRM(15,II),STRM(7,II),
     *                      STRM(19,II),SFRQ(1,II),STRM(11,II),
     *                      STRM(21,II),STRM(22,II),STRM(23,II)
Cdep       OUTTYPE 7 IS USED TO PRINT WATER CONTENT PROFILES FOR UNSATURATED FLOW.
                   CASE (7)
                     IF(IBD.NE.0) THEN
                       WRITE (IG3,280) GAGETM
                       DO IL=1,NUMAVE-1
                         WRITE (IG3,285) AVDPT(II,IL),AVWAT(II,IL),
     *                                   WAT1(II,IL)
                       END DO
                     END IF
                 END SELECT
               ELSE
C              TRANSPORT ON
C                GET OUTTYPE
                 IF (NSOL.LE.0) THEN
                     CALL USTOP(' ')
                 END IF
                 SELECT CASE (IGGLST(4,IOG))
                 CASE(0)
                  WRITE (LFRMAT,450) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),
     *                          STRM(9,II),(COUT(II,ISOL),ISOL=1,NSOL)
                 CASE(1)
                  WRITE (LFRMAT,455) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II),
     *                          (COUT(II,ISOL),ISOL=1,NSOL)
                 CASE(2)
                  WRITE (LFRMAT,460) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),STRM(9,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II),
     *                          (COUT(II,ISOL),ISOL=1,NSOL)
                 CASE(3)
                  DO 5 ISOL=1,NSOL
                     CLOAD(ISOL)=STRM(9,II)*COUT(II,ISOL)
    5             CONTINUE
                  WRITE (LFRMAT,452) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),
     *                      STRM(9,II),
     *                      (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
                 CASE(4)
                  DO 6 ISOL=1,NSOL
                     CLOAD(ISOL)=STRM(9,II)*COUT(II,ISOL)
    6             CONTINUE
                  WRITE (LFRMAT,465) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),STRM(9,II),
     *                    DEPTH,STRM(5,II),SFRQ(1,II),
     *                    STRM(16,II),STRM(17,II),STRM(18,II),
     *                    (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
C              OUTTYPE 5 IS USED TO PRINT TIME SERIES OF DIVERSIONS
                   CASE (5)
                 DO 7 ISOL=1,NSOL
                     CLOAD(ISOL)=STRM(9,II)*COUT(II,ISOL)
    7               CONTINUE
                  WRITE (LFRMAT,470) NSOL
                  WRITE (IG3,LFRMAT) GAGETM,STRM(15,II),PMXDVRT,
     *                    STRM(10,II),UPSTRFLW,
     *                    (COUT(II,ISOL),CLOAD(ISOL),ISOL=1,NSOL)
                 END SELECT
               END IF
         ELSE
            GO TO 10
         END IF
 10   CONTINUE
C
 250  FORMAT (4X,1PE11.4,2X,2(E11.4,2X))
 255  FORMAT (4X,1PE11.4,2X,5(E11.4,2X))
 260  FORMAT (4X,1PE11.4,2X,5(E11.4,2X))
 265  FORMAT (4X,1PE11.4,3X,4(E11.4,2X),2X,4(E11.4,2X))
 270  FORMAT (4X,1PE11.4,2X,2(E11.4,3X),4X,2(E11.4,4X))
 275  FORMAT (4X,1PE11.4,2X,5(E11.4,2X),3(E11.4,5X))
 280  FORMAT (4X,1PE11.4)
 285  FORMAT (16X,1PE11.3,8X,E12.4,15X,E12.4)
 450  FORMAT ('(4X,1PE11.4,1X,2(E11.4,1X),',I3,'E11.3,1X))')
 452  FORMAT ('(4X,1PE11.4,1X,2(E11.4,1X),',I3,'(2(E11.3,1X)))')
 455  FORMAT ('(4X,1PE11.4,1X,5(E11.4,1X),',I3,'(E11.3,1X))')
 460  FORMAT ('(4X,1PE11.4,1X,5(E11.4,1X),',I3,'(E11.3,1X))')
 465  FORMAT ('(4X,1PE11.4,1X,8(E11.4,1X),',I3,'(2(E11.3,1X)))')
 470  FORMAT ('(4X,1PE11.4,1X,4(E11.4,2X),',I3,'(2(E11.3,1X)))')
C
C  RELEASE MEMORY
      DEALLOCATE(CLOAD)
C
      RETURN
      END
