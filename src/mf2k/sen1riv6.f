C     Last change:  ERB  12 Jun 2000    4:10 pm
      SUBROUTINE SEN1RIV6FM(MXRIVR,RIVR,HNEW,NCOL,NROW,NLAY,IBOUND,RHS,
     &                      IP,NRIVVL)
C-----VERSION 19990323 ERB
C     ******************************************************************
C     FOR RIVER CELLS: CALCULATE MATRIX AND VECTOR DERIVATIVES, MULTIPLY
C     BY HEADS AND ADD COMPONENTS TO RHS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL FACTOR, RHS, RIVR
      INTEGER I, IBOUND, II, J, K, MXRIVR, NCOL, NLAY, NROW
      DOUBLE PRECISION DF, HNEW(NCOL,NROW,NLAY)
      DIMENSION IBOUND(NCOL,NROW,NLAY), RHS(NCOL,NROW,NLAY),
     &          RIVR(NRIVVL,MXRIVR)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      IF (IACTIVE(IP).EQ.0) RETURN
      IPOS1 = IPLOC(1,IP)
      NPC = IPLOC(2,IP)-IPOS1+1
C-------LOOP THROUGH PARAMETER CELLS
      DO 20 II = 1, NPC
        ICP = IPOS1-1+II
        K = RIVR(1,ICP)
        I = RIVR(2,ICP)
        J = RIVR(3,ICP)
        IF (IBOUND(J,I,K).GT.0) THEN
C-------CALCULATE CONTRIBUTION TO SENSITIVITY.
          ELEVR = RIVR(4,ICP)
          FACTOR = RIVR(5,ICP)
          IF (HNEW(J,I,K).GT.RIVR(6,ICP)) THEN
            DF = FACTOR*(ELEVR-HNEW(J,I,K))
          ELSE
            DF = FACTOR*(ELEVR-RIVR(6,ICP))
          ENDIF
          RHS(J,I,K) = RHS(J,I,K) - DF
        ENDIF
   20 CONTINUE
C
      RETURN
      END
