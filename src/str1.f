      SUBROUTINE STR1AL(ISUM,LENX,LCSTRM,ICSTRM,MXSTRM,NSTREM,IN,
     1                   IOUT,ISTCB1,ISTCB2,NSS,NTRIB,NDIV,ICALC,CONST,
     2                   LCTBAR,LCTRIB,LCIVAR,LCFGAR)
C                                                                      C
C-----VERSION    2 18DEC1990 STR1AL                                    C
C     *****************************************************************C
C     ALLOCATE ARRAY STORAGE FOR STREAMS                               C
C     *****************************************************************C
C                                                                      C
C     SPECIFICATIONS:                                                  C
C     -----------------------------------------------------------------C
C     -----------------------------------------------------------------C
C                                                                      C
C1------IDENTIFY PACKAGE AND INITIALIZE NSTREM.                        C
      WRITE(IOUT,1) IN
    1 FORMAT(1H0,'STRM -- STREAM PACKAGE, VERSION 2, 12/18/90 ',
     1'INPUT READ FROM UNIT',I3)
      NSTREM=0
C                                                                      C
C2------ READ MXSTRM, NSS, NTRIB, ISTCB1, AND ISTCB2.                  C
  100 READ(IN,3)MXSTRM,NSS,NTRIB,NDIV,ICALC,CONST,ISTCB1,ISTCB2
    3 FORMAT(5I10,F10.0,2I10)
      IF(MXSTRM.LT.0)MXSTRM=0
      IF(NSS.LT.0)NSS=0
      WRITE(IOUT,4)MXSTRM,NSS,NTRIB
    4 FORMAT(1H ,'MAXIMUM OF',I5,' STREAM NODES'//1X,'NUMBER OF STREAM S
     1EGMENTS IS ',I5//1X,'NUMBER OF STREAM TRIBUTARIES IS ',I5//)
      IF(NDIV.GT.0) WRITE(IOUT,5)
    5 FORMAT(1H ,'DIVERSIONS FROM STREAMS HAVE BEEN SPECIFIED')
      IF(ICALC.GT.0) WRITE(IOUT,6) CONST
    6 FORMAT(1H ,'STREAM STAGES WILL BE CALCULATED USING A CONSTANT OF
     1',F10.4)
      IF(ISTCB1.GT.0) WRITE(IOUT,7) ISTCB1,ISTCB2
    7 FORMAT(1X,'CELL BUDGETS WILL BE SAVED ON UNITS',I3,'AND',I3)
C                                                                      C
C3------SET LCSTRM EQUAL TO ADDRESS OF FIRST UNUSED SPACE IN X.        C
  200 LCSTRM=ISUM
C                                                                      C
C4------CALCULATE AMOUNT OF SPACE NEEDED FOR STRM LIST.                C
      ISPA=11*MXSTRM
      ISUM=ISUM+ISPA
C                                                                      C
C5------CALCULATE AMOUNT OF SPACE NEEDED FOR ISTRM LIST.               C
      ICSTRM=ISUM
      ISPB=5*MXSTRM
      ISUM=ISUM+ISPB
C                                                                      C
C6------CALCULATE AMOUNT OF SPACE NEEEDED FOR ITRBAR LIST.             C
      LCTBAR=ISUM
      ISPC=NSS*NTRIB
      ISUM=ISUM+ISPC
C                                                                      C
C7------CALCULATE AMOUNT OF SPACE NEEDED FOR ARTRIB LIST.              C
      LCTRIB=ISUM
      ISPD=NSS
      ISUM=ISUM+ISPD
C                                                                      C
C8A-----CALCULATE AMOUNT OF SPACE NEEDED FOR IDIVAR LIST.              C
      LCIVAR=ISUM
      ISPE=NSS
      ISUM=ISUM+ISPE
C                                                                      C
C8B-----CALCULATE AMOUNT IF SPACE NEEDED FOR NDFGAR LIST.              C
      LCFGAR=ISUM
      ISPF=NSS
      ISUM=ISUM+ISPF
      ISP=ISPA+ISPB+ISPC+ISPD+ISPE+ISPF
C                                                                      C
C9------PRINT AMOUNT OF SPACE USED BY STREAM PACKAGE.                  C
      WRITE (IOUT,8)ISP
    8 FORMAT(1X,I8,' ELEMENTS IN X ARRAY ARE USED FOR STREAMS')
      ISUM1=ISUM-1
      WRITE(IOUT,9)ISUM1,LENX
    9 FORMAT(1X,I8,' ELEMENTS OF X ARRAY USED OUT OF',I7)
      IF(ISUM1.GT.LENX) WRITE(IOUT,10)
   10 FORMAT(1X,'   ***X ARRAY MUST BE DIMENSIONED LARGER***')
C                                                                      C
C10-----RETURN.                                                        C
      RETURN
      END
      SUBROUTINE STR1RP(STRM,ISTRM,NSTREM,MXSTRM,IN,IOUT,ITRBAR,NDIV,
     1                   NSS,NTRIB,IDIVAR,ICALC,IPTFLG)
C                                                                      C
C                                                                      C
C-----VERSION  2  18DEC1990 STR1RP                                     C
C     *****************************************************************C
C     READ STREAM DATA:  INCLUDES SEGMENT AND REACH NUMBERS, CELL      C
C         SEQUENCE OF SEGMENT AND REACH, FLOW INTO MODEL AT BOUNDARY,  C
C         STREAM STAGE, STREAMBED CONDUCTANCE, AND STREAMBED TOP AND   C
C         BOTTOM ELEVATIONS                                            C
C     *****************************************************************C
C                                                                      C
C     SPECIFICATIONS:                                                  C
C     -----------------------------------------------------------------C
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM),ITRBAR(NSS,NTRIB),
     1          IDIVAR(NSS)
C     -----------------------------------------------------------------C
C                                                                      C
C1A-----IF MXSTREAM IS LESS THAN 1 THEN STREAM IS INACTIVE. RETURN.    C
      IF(MXSTRM.LT.1) RETURN
C                                                                      C
C1B-----READ ITMP(NUMBER OF STREAM CELLS OR FLAG TO REUSE DATA).       C
      READ(IN,1)ITMP,IRDFLG,IPTFLG
    1 FORMAT(3I10)
C                                                                      C
C2A-----IF ITMP <0 THEN REUSE DATA FROM LAST STRESS PERIOD.            C
      IF(ITMP.GE.0)GO TO 50
      WRITE(IOUT,2)
    2 FORMAT(1H0,'REUSING STREAM NODES FROM LAST STRESS PERIOD')
      RETURN
C                                                                      C
C2B-----IF ITMP=> ZERO THEN IT IS THE NUMBER OF STREAM REACHES.        C
   50 NSTREM=ITMP
C                                                                      C
C3A-----IF NSTREM>MXSTRM THEN STOP.                                    C
      IF(NSTREM.LE.MXSTRM)GO TO 100
      WRITE(IOUT,99)NSTREM,MXSTRM
   99 FORMAT(1H0,'NSTREM(',I4,') IS GREATER THAN MXSTRM(',I4,')')
      STOP
C                                                                      C
C3B-----PRINT NUMBER OF STREAM CELLS IN THIS STRESS PERIOD.            C
  100 IF(IRDFLG.EQ.0) WRITE(IOUT,3)NSTREM
    3 FORMAT(1H0,//1X,I5,' STREAM NODES')
C                                                                      C
C4------IF THERE ARE NO STREAM CELLS THEN RETURN.                      C
      IF(NSTREM.EQ.0) RETURN
C                                                                      C
C5------READ AND PRINT DATA FOR EACH STREAM CELL.                      C
      IF(IRDFLG.EQ.0) WRITE(IOUT,4)
    4 FORMAT(1H ,3X,'LAYER   ROW    COL    SEGMENT   REACH   STREAMFLOW
     1     STREAM    STREAMBED     STREAMBED BOT  STREAMBED TOP',/27X,
     2'NUMBER   NUMBER                   STAGE   CONDUCTANCE      ELEVAT
     3ION      ELEVATION',/3X,110('-'))
      DO 250 II=1,NSTREM
      READ(IN,5)K,I,J,ISTRM(4,II),ISTRM(5,II),STRM(1,II),STRM(2,II),
     1STRM(3,II),STRM(4,II),STRM(5,II)
    5 FORMAT(5I5,F15.0,4F10.0)
      IF(IRDFLG.EQ.0) WRITE(IOUT,6)K,I,J,ISTRM(4,II),ISTRM(5,II),
     1STRM(1,II),STRM(2,II),STRM(3,II),STRM(4,II),STRM(5,II)
    6 FORMAT(1X,3X,I4,2I7,2I9,7X,G11.4,G12.4,G11.4,4X,2G13.4)
      ISTRM(1,II)=K
      ISTRM(2,II)=I
      ISTRM(3,II)=J
  250 CONTINUE
C                                                                      C
C6----READ AND PRINT DATA IF STREAM STAGE IS CALCULATED.               C
      IF(ICALC.LE.0) GO TO 300
      IF(IRDFLG.EQ.0) WRITE(IOUT,7)
    7 FORMAT(1H0,3X,'LAYER',3X,'ROW',4X,'COL   ',' SEGMENT',3X,
     1'REACH',8X,'STREAM',13X,'STREAM',10X,'ROUGH',/27X,'NUMBER',3X,
     2 'NUMBER',8X,'WIDTH',14X,'SLOPE',10X,'COEF.',/3X,110('-'))
      DO 280 II=1,NSTREM
      READ(IN,8) STRM(6,II),STRM(7,II),STRM(8,II)
    8 FORMAT(3F10.0)
      IF(IRDFLG.EQ.0) WRITE(IOUT,9)ISTRM(1,II),ISTRM(2,II),ISTRM(3,II),
     1ISTRM(4,II),ISTRM(5,II),STRM(6,II),STRM(7,II),STRM(8,II)
    9 FORMAT(4X,I4,2I7,2I9,7X,G12.4,4X,G13.4,4X,G12.4)
  280 CONTINUE
C                                                                      C
C7------INITIALIZE ALL TRIBUTARY SEGMENTS TO ZERO.                     C
  300 DO 320 IK=1,NSS
      DO 320 JK=1,NTRIB
      ITRBAR(IK,JK)=0
  320 CONTINUE
C                                                                      C
C8-----INITIALIZE DIVERSION SEGMENT ARRAY TO ZERO.                     C
      DO 325 IK=1,NSS
      IDIVAR(IK)=0
  325 CONTINUE
C                                                                      C
C9-----READ AND PRINT TRIBUTARY SEGMENTS.                              C
      IF(NTRIB.LE.0) GO TO 343
      IF(IRDFLG.EQ.0) WRITE(IOUT,10)NTRIB
   10 FORMAT(1H0,30X,'MAXIMUM NUMBER OF TRIBUTARY STREAMS IS ',I5,//1X,
     1 20X,'STREAM SEGMENT',15X,'TRIBUTARY STREAM SEGMENT NUMBERS')
      DO 340 IK=1,NSS
      READ(IN,11) (ITRBAR(IK,JK),JK=1,NTRIB)
   11 FORMAT(10I5)
      IF(IRDFLG.EQ.0) WRITE(IOUT,12)IK,(ITRBAR(IK,JK),JK=1,NTRIB)
   12 FORMAT(20X,I5,20X,10I5)
  340 CONTINUE
C                                                                      C
C10----READ AND PRINT DIVERSION SEGMENTS NUMBERS.                      C
  343 IF(NDIV.LE.0) GO TO 350
      IF(IRDFLG.EQ.0) WRITE(IOUT,13)
   13 FORMAT(1H0,10X,'DIVERSION SEGMENT NUMBER',10X,
     1       'UPSTREAM SEGMENT NUMBER')
      DO 345 IK=1,NSS
      READ(IN,14) IDIVAR(IK)
   14 FORMAT(I10)
      IF(IRDFLG.EQ.0) WRITE(IOUT,15) IK,IDIVAR(IK)
   15 FORMAT(20X,I5,28X,I5)
  345 CONTINUE
C                                                                      C
C11----SET FLOW OUT OF REACH, FLOW INTO REACH, AND FLOW THROUGH        C
C      STREAM BED TO ZERO.                                             C
  350 DO 360 II =1,NSTREM
      STRM(9,II)=0.0
      STRM(10,II)=0.0
      STRM(11,II)=0.0
  360 CONTINUE
C                                                                      C
C12------RETURN                                                        C
      RETURN
      END
      SUBROUTINE STR1FM(NSTREM,STRM,ISTRM,HNEW,HCOF,RHS,IBOUND,MXSTRM,
     1                  NCOL,NROW,NLAY,IOUT,NSS,ITRBAR,NTRIB,ARTRIB,
     2                  IDIVAR,NDFGAR,ICALC,CONST)
C                                                                      C
C-----VERSION   2 18DEC1990 STR1FM                                     C
C                                                                      C
C     *****************************************************************C
C     ADD STREAM TERMS TO RHS AND HCOF IF FLOW OCCURS IN MODEL CELL    C
C     *****************************************************************C
C                                                                      C
C     SPECIFICATIONS:                                                  C
C     -----------------------------------------------------------------C
C                                                                      C
      DOUBLE PRECISION HNEW
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM),HNEW(NCOL,NROW,NLAY),
     1          HCOF(NCOL,NROW,NLAY),RHS(NCOL,NROW,NLAY),
     2          IBOUND(NCOL,NROW,NLAY),ITRBAR(NSS,NTRIB),ARTRIB(NSS),
     3          IDIVAR(NSS),NDFGAR(NSS)
C     -----------------------------------------------------------------C
C                                                                      C
C1------IF NSTREM<=0 THERE ARE NO STREAMS. RETURN.                     C
      IF(NSTREM.LE.0)RETURN
C                                                                      C
C2A-----PROCESS EACH CELL IN THE STREAM LIST.                          C
C2B-----INITIALIZE NDFGAR ARRAY TO ZERO.                               C
      DO 5 I=1,NSS
      NDFGAR(I)=0
    5 CONTINUE
C                                                                      C
C3------DETERMINE LAYER, ROW, COLUMN OF EACH REACH.                    C
      DO 500 L=1,NSTREM
      LL=L-1
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
C                                                                      C
C4----06FEB1990, CHECK FOR CELLS OUTSIDE MOVED TO C12, C16 AND C18.    C
C                                                                      C
C5------DETERMINE STREAM SEGMENT AND REACH NUMBER.                     C
      ISTSG=ISTRM(4,L)
      NREACH=ISTRM(5,L)
C                                                                      C
C6------SET FLOWIN EQUAL TO STREAM SEGMENT INFLOW IF FIRST REACH.      C
      IF(NREACH.GT.1) GO TO 200
      FLOWIN=STRM(1,L)
C                                                                      C
C7------STORE OUTFLOW FROM PREVIOUS SEGMENT IN ARTRIB IF SEGMENT >1.   C
      IF(ISTSG.EQ.1)GO TO 50
      IFLG = ISTRM(4,LL)
      ARTRIB(IFLG)=STRM(9,LL)
C                                                                      C
C8A-----CHECK UPSTREAM SEGMENT FOR DIVERSIONS.                         C
      DO 40 NSFLG = 1,NSS
      IF(IFLG.NE.IDIVAR(NSFLG)) GO TO 40
C                                                                      C
C8B-----DETERMINE AMOUNT OF FLOW TO BE DIVERTED.                       C
      DO 20 IDL=1,NSTREM
      IF(NSFLG.NE.ISTRM(4,IDL)) GO TO 20
      IF(ISTRM(5,IDL).NE.1) GO TO 20
      DUM=ARTRIB(IFLG)-STRM(1,IDL)
C                                                                      C
C8C-----SUBTRACT FLOW FROM UPSTREAM SEGMENT IF THERE IS ENOUGH FLOW    C
C-------IN UPSTREAM SEGMENT.                                           C
      IF(DUM.GE.0.0) ARTRIB(IFLG)=DUM
      IF(DUM.LT.0.0) NDFGAR(IFLG)=1
   20 CONTINUE
   40 CONTINUE
   50 IF(IDIVAR(ISTSG).LE.0) GO TO 60
      NDFLG=IDIVAR(ISTSG)
      IF(NDFGAR(NDFLG).EQ.1) FLOWIN=0.0
   60 IF(FLOWIN.GE.0.0) GO TO 300
C                                                                      C
C9-----SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.C
      FLOWIN =0.
      DO 100 ITRIB=1,NTRIB
      INODE=ITRBAR(ISTSG,ITRIB)
      IF(INODE.LE.0) GO TO 100
      FLOWIN=FLOWIN+ARTRIB(INODE)
  100 CONTINUE
C                                                                      C
C10-----IF REACH >1, SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH.  C
  200 IF(NREACH.GT.1) FLOWIN=STRM(9,LL)
C                                                                      C
C11----COMPUTE STREAM STAGE IN REACH IF ICALC IS GREATER THAN 1.       C
  300 IF(ICALC.LE.0) GO TO 310
      XNUM=((FLOWIN+STRM(9,L))/2.0)*STRM(8,L)
      DNOM=CONST*STRM(6,L)*(SQRT(STRM(7,L)))
      DEPTH=(XNUM/DNOM)**0.6
      IF(DEPTH.LE.0.) DEPTH=0.
      STRM(2,L)=DEPTH+STRM(5,L)
  310 HSTR=STRM(2,L)
C                                                                      C
C12----DETERMINE LEAKAGE THROUGH STREAMBED.                            C
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 315
      IF(FLOWIN.LE.0.) HSTR=STRM(5,L)
      CSTR=STRM(3,L)
      SBOT=STRM(4,L)
      H=HNEW(IC,IR,IL)
      T=HSTR-SBOT
C                                                                      C
C13----COMPUTE LEAKAGE AS A FUNCTION OF STREAM STAGE AND HEAD IN CELL. C
      FLOBOT=CSTR*(HSTR-H)
C                                                                      C
C14----RECOMPUTE LEAKAGE IF HEAD IN CELL IS BELOW STREAMBED BOTTOM.    C
      IQFLG=0
      IF(H.GT.SBOT) GO TO 312
      IQFLG=1
      FLOBOT=CSTR*T
C                                                                      C
C15----SET LEAKAGE EQUAL TO STREAM INFLOW IF LEAKAGE MORE THAN INFLOW. C
  312 IF(FLOBOT.LE.FLOWIN) GO TO 320
      IQFLG=1
      FLOBOT=FLOWIN
C                                                                      C
C16-----STREAMFLOW OUT EQUALS STREAMFLOW IN MINUS LEAKAGE.             C
  315 IF(IBOUND(IC,IR,IL).LE.0) FLOBOT=0.
  320 FLOWOT=FLOWIN-FLOBOT
      IF((ISTSG.GT.1).AND.(NREACH.EQ.1)) STRM(9,LL)=ARTRIB(IFLG)
C                                                                      C
C17----STORE STREAM INFLOW, OUTFLOW AND LEAKAGE FOR EACH REACH.        C
      STRM(9,L)=FLOWOT
      STRM(10,L)=FLOWIN
      STRM(11,L)=FLOBOT
C                                                                      C
C18----RETURN TO STEP 3 IF STREAM INFLOW IS LESS THAN OR EQUAL TO ZERO C
C       AND LEAKAGE IS GREATER THAN OR EQUAL TO ZERO OR IF CELL        C
C       IS NOT ACTIVE--IBOUND IS LESS THAN OR EQUAL TO ZERO.           C
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 500
      IF((FLOWIN.LE.0.0).AND.(FLOBOT.GE.0.0)) GO TO 500
C                                                                      C
C19------IF HEAD > BOTTOM THEN ADD TERMS TO RHS AND HCOF.              C
      IF(IQFLG.GT.0) GO TO 400
      RHS(IC,IR,IL)=RHS(IC,IR,IL)-CSTR*HSTR
      HCOF(IC,IR,IL)=HCOF(IC,IR,IL)-CSTR
      GO TO 500
C                                                                      C
C20------IF HEAD < BOTTOM THEN ADD TERM ONLY TO RHS.                   C
  400 RHS(IC,IR,IL)=RHS(IC,IR,IL)-FLOBOT
  500 CONTINUE
C                                                                      C
C21-----RETURN.                                                        C
      RETURN
      END
      SUBROUTINE STR1BD(NSTREM,STRM,ISTRM,IBOUND,MXSTRM,HNEW,NCOL,NROW,
     1  NLAY,DELT,VBVL,VBNM,MSUM,KSTP,KPER,ISTCB1,ISTCB2,ICBCFL,BUFF,
     2  IOUT,NTRIB,NSS,ARTRIB,ITRBAR,IDIVAR,NDFGAR,ICALC,CONST,IPTFLG)
C-----VERSION  2  18DEC1990 STR1BD                                     C
C                                                                      C
C     *****************************************************************C
C     CALCULATE VOLUMETRIC BUDGET FOR STREAMS                          C
C     *****************************************************************C
C                                                                      C
C     SPECIFICATIONS:                                                  C
C     -----------------------------------------------------------------C
      CHARACTER*4 VBNM,TEXT,STRTXT
      DOUBLE PRECISION HNEW
      DIMENSION STRM(11,MXSTRM),ISTRM(5,MXSTRM),IBOUND(NCOL,NROW,NLAY),
     1          HNEW(NCOL,NROW,NLAY),VBVL(4,20),VBNM(4,20),
     2          BUFF(NCOL,NROW,NLAY),ARTRIB(NSS),ITRBAR(NSS,NTRIB),
     3             IDIVAR(NSS),NDFGAR(NSS)
      DIMENSION TEXT(4),STRTXT(4)
      DATA TEXT(1),TEXT(2),TEXT(3),TEXT(4) /'  ST','REAM',' LEA','KAGE'/
      DATA STRTXT(1),STRTXT(2),STRTXT(3),STRTXT(4) /'STRE','AM F',
     1                                              'LOW ','OUT '/
C     -----------------------------------------------------------------C
C                                                                      C
C1------SET IBD=1 IF BUDGET TERMS SHOULD BE SAVED ON DISK.             C
      IBD=0
      RATIN = 0.
      RATOUT = 0.
C                                                                      C
C2------IF NO REACHES, KEEP ZEROS IN ACCUMULATORS.                     C
      IF(NSTREM.EQ.0) GO TO 600
C                                                                      C
C3A-----TEST TO SEE IF CELL-BY-CELL TERMS ARE NEEDED.                  C
      IF((ICBCFL.EQ.0).OR.(ISTCB1.LE.0)) GO TO 10
C                                                                      C
C3B-----CELL-BY-CELL TERMS ARE NEEDED, SET IBD AND CLEAR BUFFER.       C
      IBD = 1
      DO 5 IL=1,NLAY
      DO 5 IR=1,NROW
      DO 5 IC=1,NCOL
      BUFF(IC,IR,IL)=0.
    5 CONTINUE
C                                                                      C
C3C-----INITIALIZE NDFGAR ARRAY TO ZERO.                               C
      DO 7 I=1,NSS
      NDFGAR(I)=0
    7 CONTINUE
C                                                                      C
C4------IF THERE ARE STREAMS THEN ACCUMULATE LEAKAGE TO OR FROM THEM.  C
   10 DO 500 L=1,NSTREM
      LL=L-1
C                                                                      C
C5---DETERMINE REACH LOCATION.                                         C
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
C                                                                      C
C6----06FEB1990, CHECK FOR CELLS OUTSIDE MOVED TO C14, C18 AND C29.    C
C                                                                      C
C7------DETERMINE SEGMENT AND REACH NUMBER.                            C
      ISTSG=ISTRM(4,L)
      NREACH=ISTRM(5,L)
      IF(NREACH.GT.1) GO TO 200
C                                                                      C
C8------SET FLOWIN EQUAL TO SEGMENT INFLOW IF FIRST REACH.             C
      FLOWIN=STRM(1,L)
C                                                                      C
C9------STORE OUTFLOW FROM PREVIOUS SEGMENT IN ARTRIB IF SEGMENT >1.   C
      IF(ISTSG.EQ.1) GO TO 50
      IFLG = ISTRM(4,LL)
      ARTRIB(IFLG)=STRM(9,LL)
C                                                                      C
C10A----CHECK UPSTREAM SEGMENT FOR DIVERSIONS.                         C
      DO 40 NSFLG = 1,NSS
      IF(IFLG.NE.IDIVAR(NSFLG)) GO TO 40
C                                                                      C
C10B----DETERMINE AMOUNT OF FLOW TO BE DIVERTED.                       C
      DO 20 IDL=1,NSTREM
      IF(NSFLG.NE.ISTRM(4,IDL)) GO TO 20
      IF(ISTRM(5,IDL).NE.1) GO TO 20
      DUM=ARTRIB(IFLG)-STRM(1,IDL)
C                                                                      C
C10C----SUBTRACT FLOW FROM UPSTREAM SEGMENT IF THERE IS ENOUGH FLOW    C
C       IN UPSTREAM SEGMENT.                                           C
      IF(DUM.GE.0.0) ARTRIB(IFLG)=DUM
      IF(DUM.LT.0.0) NDFGAR(IFLG)=1
   20 CONTINUE
   40 CONTINUE
   50 IF(IDIVAR(ISTSG).LE.0) GO TO 60
      NDFLG=IDIVAR(ISTSG)
      IF(NDFGAR(NDFLG).EQ.1) FLOWIN=0.0
   60 IF(FLOWIN.GE.0.0) GO TO 300
C                                                                      C
C11--SUM TRIBUTARY OUTFLOW AND USE AS INFLOW INTO DOWNSTREAM SEGMENT.  C
      FLOWIN =0.
      DO 100 ITRIB=1,NTRIB
      INODE=ITRBAR(ISTSG,ITRIB)
      IF(INODE.LE.0) GO TO 100
      FLOWIN=FLOWIN+ARTRIB(INODE)
  100 CONTINUE
C                                                                      C
C12-----IF REACH >1, SET INFLOW EQUAL TO OUTFLOW FROM UPSTREAM REACH.  C
  200 IF(NREACH.GT.1) FLOWIN=STRM(9,LL)
C                                                                      C
C13----COMPUTE STREAM STAGE IN REACH IF ICALC > 1.                     C
  300 IF(ICALC.LE.0) GO TO 310
      XNUM=((FLOWIN+STRM(9,L))/2.0)*STRM(8,L)
      DNOM=CONST*STRM(6,L)*(SQRT(STRM(7,L)))
      DEPTH=(XNUM/DNOM)**0.6
      IF((DEPTH).LE.0) DEPTH=0.
      STRM(2,L)=DEPTH+STRM(5,L)
  310 HSTR=STRM(2,L)
C                                                                      C
C14----DETERMINE LEAKAGE THROUGH STREAMBED.                            C
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 315
      IF(FLOWIN.LE.0.0) HSTR=STRM(5,L)
      CSTR=STRM(3,L)
      SBOT=STRM(4,L)
      H=HNEW(IC,IR,IL)
      T=HSTR-SBOT
C                                                                      C
C15----COMPUTE LEAKAGE AS A FUNCTION OF STREAM STAGE AND HEAD IN CELL. C
      FLOBOT=CSTR*(HSTR-H)
C                                                                      C
C16----RECOMPUTE LEAKAGE IF HEAD IN CELL IS BELOW STREAMBED BOTTOM.    C
      IF(H.GT.SBOT) GO TO 312
      FLOBOT=CSTR*T
C                                                                      C
C17----SET LEAKAGE EQUAL TO STREAM INFLOW IF LEAKAGE MORE THAN INFLOW. C
  312 IF(FLOBOT.LE.FLOWIN) GO TO 320
      FLOBOT=FLOWIN
C                                                                      C
C18----STREAMFLOW OUT EQUALS STREAMFLOW IN MINUS LEAKAGE.              C
  315 IF(IBOUND(IC,IR,IL).LE.0) FLOBOT=0.
  320 FLOWOT=FLOWIN-FLOBOT
      IF((ISTSG.GT.1).AND.(NREACH.EQ.1)) STRM(9,LL)=ARTRIB(IFLG)
C                                                                      C
C19----STORE STREAM INFLOW, OUTFLOW AND LEAKAGE FOR EACH REACH.        C
      STRM(9,L)=FLOWOT
      STRM(10,L)=FLOWIN
      STRM(11,L)=FLOBOT
C                                                                      C
C20----IF LEAKAGE FROM STREAMS IS TO BE SAVED THEN ADD RATE TO BUFFER.  C
      IF(IBD.EQ.1) BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+FLOBOT
C                                                                      C
C21----DETERMINE IF FLOW IS INTO OR OUT OF MODEL CELL.                 C
C       SKIP ESTIMATE OF LEAKAGE FROM STREAM IF LEAKAGE IS ZERO.       C
      IF(FLOBOT)494,500,496
C                                                                      C
C22-----SUBTRACT FLOW RATE FROM RATOUT IF AQUIFER DISCHARGES TO STREAM.C
  494 RATOUT=RATOUT-FLOBOT
      GO TO 500
C                                                                      C
C23-----ADD FLOW RATE TO RATIN IF STREAM DISCHARGES TO AQUIFER.        C
  496 RATIN=RATIN+FLOBOT
  500 CONTINUE
C                                                                      C
C24-----IF BUDGET TERMS WILL BE SAVED THEN WRITE TO DISK.              C
      IF(IBD.EQ.1) CALL UBUDSV(KSTP,KPER,TEXT,ISTCB1,BUFF,NCOL,NROW,
     1                          NLAY,IOUT)
C                                                                      C
C25A-----MOVE RATES INTO VBVL FOR PRINTING BY MODULE BAS_OT.           C
  600 VBVL(3,MSUM)=RATIN
      VBVL(4,MSUM)=RATOUT
C                                                                      C
C25B-----MOVE PRODUCT OF RATE AND TIME STEP INTO VBVL ACCUMULATORS.    C
      VBVL(1,MSUM)=VBVL(1,MSUM)+RATIN*DELT
      VBVL(2,MSUM)=VBVL(2,MSUM)+RATOUT*DELT
C                                                                      C
C25C-----MOVE BUDGET TERM LABELS INTO VBNM FOR PRINTING BY BAS_OT.     C
      VBNM(1,MSUM)=TEXT(1)
      VBNM(2,MSUM)=TEXT(2)
      VBNM(3,MSUM)=TEXT(3)
      VBNM(4,MSUM)=TEXT(4)
C                                                                      C
C26-----INCREASE BUDGET TERM COUNTER BY ONE.                           C
      MSUM=MSUM+1
C                                                                      C
C27-----RESET IBD COUNTER TO ZERO.                                     C
      IBD=0
C28----IF STREAM OUTFLOW FROM EACH REACH IS TO BE STORED ON DISK       C
C     THEN STORE OUTFLOW RATES TO BUFFER.                              C
      IF((ICBCFL.EQ.0).OR.(ISTCB2.LE.0)) GO TO 625
      IBD = 1
      DO 605 IL=1,NLAY
      DO 605 IR=1,NROW
      DO 605 IC=1,NCOL
  605 BUFF(IC,IR,IL)=0.
C                                                                      C
C29-----SAVE STREAMFLOWS OUT OF EACH REACH ON DISK.                    C
      DO 615 L=1,NSTREM
      IC=ISTRM(3,L)
      IR=ISTRM(2,L)
      IL=ISTRM(1,L)
      IF(IBOUND(IC,IR,IL).LE.0) GO TO 615
      BUFF(IC,IR,IL)=BUFF(IC,IR,IL)+STRM(9,L)
  615 CONTINUE
      CALL UBUDSV(KSTP,KPER,STRTXT,ISTCB2,BUFF,NCOL,NROW,NLAY,IOUT)
C                                                                      C
C30-----PRINT STREAMFLOW RATES AND LEAKAGE FOR EACH REACH.             C
  625 IF((ISTCB1.GE.0).OR.(ICBCFL.LE.0)) GO TO 800
      IF(IPTFLG.GT.0) GO TO 800
      IF(ICALC.GT.0) GO TO 700
      WRITE(IOUT,650)
  650 FORMAT(1H0,12X,'LAYER',6X,'ROW',5X,'COLUMN',5X,'STREAM',4X,
     1'REACH',6X,'FLOW INTO',4X,'FLOW INTO',6X,'FLOW OUT OF'/43X,
     2      'NUMBER',3X,'NUMBER',4X,'STREAM REACH',4X,'AQUIFER',
     3      6X,'STREAM REACH'//)
      DO 690 L=1,NSTREM
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
      WRITE(IOUT,675)IL,IR,IC,ISTRM(4,L),ISTRM(5,L),
     1     STRM(10,L),STRM(11,L),STRM(9,L)
  675 FORMAT(1X,5X,5I10,8X,G9.3,5X,G9.3,8X,G9.3)
  690 CONTINUE
      GO TO 800
  700 WRITE(IOUT,710)
  710 FORMAT(1H0,12X,'LAYER',6X,'ROW',5X,'COLUMN',5X,'STREAM',4X,
     1'REACH',6X,'FLOW INTO',4X,'FLOW INTO',6X,'FLOW OUT OF',5X,
     2'HEAD IN'/43X,      'NUMBER',3X,'NUMBER',4X,'STREAM REACH',
     3 4X,'AQUIFER',6X,'STREAM REACH',5X,'STREAM'//)
      DO 750 L=1,NSTREM
      IL=ISTRM(1,L)
      IR=ISTRM(2,L)
      IC=ISTRM(3,L)
      WRITE(IOUT,775)IL,IR,IC,ISTRM(4,L),ISTRM(5,L),
     1     STRM(10,L),STRM(11,L),STRM(9,L),STRM(2,L)
  775 FORMAT(1X,5X,5I10,8X,G9.3,5X,G9.3,7X,G9.3,4X,F9.2)
  750 CONTINUE
  800 CONTINUE
C                                                                      C
C31-----RETURN.                                                        C
      RETURN
      END
