C     Last change:  ERB  12 Jun 2000    4:07 pm
      SUBROUTINE SEN1DRN6FM(MXDRN,DRAI,HNEW,NCOL,NROW,NLAY,IBOUND,RHS,
     &                      IP,NDRNVL)
C-----VERSION 19990323 ERB
C     ******************************************************************
C     FOR DRAIN CELLS : CALCULATE MATRIX AND VECTOR DERIVATIVES,
C     MULTIPLY BY HEADS, AND ADD COMPONENTS TO RHS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL DRAI, RHS
      INTEGER I, IBOUND, II, J, K, MXDRN, NCOL, NLAY, NROW
      DOUBLE PRECISION DF, HNEW(NCOL,NROW,NLAY)
      DIMENSION DRAI(NDRNVL,MXDRN), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      IF (IACTIVE(IP).EQ.0) RETURN
      IPOS1 = IPLOC(1,IP)
      NPC = IPLOC(2,IP)-IPOS1+1
C-----LOOP THROUGH PARAMETER CELLS
      DO 20 II = 1, NPC
        ICP = IPOS1-1+II
        K = DRAI(1,ICP)
        I = DRAI(2,ICP)
        J = DRAI(3,ICP)
        IF (IBOUND(J,I,K).GT.0) THEN
C-------CALCULATE CONTRIBUTION TO SENSITIVITY.
          ELEVD = DRAI(4,ICP)
          IF (HNEW(J,I,K).GT.ELEVD) THEN
            DF = DRAI(5,ICP)*(ELEVD-HNEW(J,I,K))
            RHS(J,I,K) = RHS(J,I,K) - DF
          ENDIF
        ENDIF
   20 CONTINUE
C
      RETURN
      END
