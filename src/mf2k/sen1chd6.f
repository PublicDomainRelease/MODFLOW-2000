C     Last change:  ERB  18 May 2000   10:54 am
      SUBROUTINE SEN1CHD6FM(MXCHD,CHDS,HNEW,PERLEN,PERTIM,
     1            NCOL,NROW,NLAY,NCHDVL,IOUT,IP,IERR,IERRU)
C
C     VERSION 20000211 ERB
C     ******************************************************************
C     COMPUTE HEAD FOR TIME STEP AT EACH TIME-VARIANT SPECIFIED HEAD
C     CELL
C     ******************************************************************
C
C     SPECIFICATIONS:
C     ------------------------------------------------------------------
      INCLUDE 'param.inc'
      DOUBLE PRECISION HNEW
      DIMENSION CHDS(NCHDVL,MXCHD),HNEW(NCOL,NROW,NLAY)
C     ------------------------------------------------------------------
C2------COMPUTE PROPORTION OF STRESS PERIOD TO CENTER OF THIS TIME STEP
      IF (PERLEN.EQ.0.0) THEN
        FRAC=1.0
      ELSE
        FRAC=PERTIM/PERLEN
      ENDIF
C
C2------PROCESS EACH ENTRY IN THE SPECIFIED-HEAD CELL LIST (CHDS)
      DO 100 L=IPLOC(1,IP),IPLOC(2,IP)
C
C3------GET COLUMN, ROW AND LAYER OF CELL CONTAINING BOUNDARY
        IL=CHDS(1,L)
        IR=CHDS(2,L)
        IC=CHDS(3,L)
C
        IF (PERLEN.EQ.0.0 .AND. CHDS(4,L).NE.CHDS(5,L)) THEN
          IF (IERR.EQ.0) IERR = -1
          WRITE(IOUT,200)IL,IR,IC
          WRITE(IERRU,200)IL,IR,IC
  200     FORMAT(/,' ***WARNING***  FOR CHD CELL (',I3,',',I3,',',I3,
     &'), START HEAD AND END HEAD DIFFER',/,
     &' FOR A STRESS PERIOD OF ZERO LENGTH --',/,
     &' USING ENDING HEAD AS CONSTANT HEAD',
     &' (SEN1CHD6FM)',/)
        ENDIF
C5------COMPUTE HEAD AT CELL BY LINEAR INTERPOLATION.
        FAC=CHDS(4,L)+(CHDS(5,L)-CHDS(4,L))*FRAC
C
C6------UPDATE THE APPROPRIATE HNEW VALUE
          HNEW(IC,IR,IL)=FAC

  100 CONTINUE
C
C7------RETURN
      RETURN
      END
