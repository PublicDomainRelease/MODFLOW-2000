C     Version 1.02
C     Last change:  ERB   9 Jul 2001    3:26 pm
      SUBROUTINE SEN1HUF1FM(H,NCOL,NROW,NLAY,PID,HK,HKCC,DELR,
     &                     DELC,IBOUND,RHS,CV,BOTM,NBOTM,HUFTHK,
     &                     NHUF,IP,IZON,NZONAR,RMLT,NMLTAR,IUHFBP,
     &                     HFBP,MXACTFB,NHFB,HOLD,DELT,ISS,IOUT)
C     ******************************************************************
C      CALCULATE MATRIX DERIVATIVES AND MULTIPLY BY HEADS AS NEEDED.  
C      ADD RESULTING CONTRIBUTION TO RHS.
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, CV,  DELC, DELR, C, DELT, HH, R0, RHS, RMLT, RMLT0, TH0,
     &     ZERO, ONE, BOTM , MID1 , MID2, TH0L, TH1L
      INTEGER I, IBM, IBOUND, IBP, IND, ISS, J, K, LT, IZON, NCOL, NLAY,
     &        NRC, NROW
      CHARACTER*4 PID
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HO , HP
      DIMENSION DELR(NCOL), DELC(NROW),
     & RHS(NCOL,NROW,NLAY), IBOUND(NCOL,NROW,NLAY),
     & CV(NCOL,NROW,NLAY),BOTM(NCOL,NROW,0:NBOTM),
     & HK(NCOL,NROW,NLAY), HUFTHK(NCOL,NROW,NHUF,2),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR),
     & HFBP(7,MXACTFB),HOLD(NCOL*NROW*NLAY),HKCC(NCOL,NROW,NLAY)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      ONE = 1.0
      NRC = NROW*NCOL
C
      DO 140 ICL = IPLOC(1,IP),IPLOC(2,IP)
        NU = IPCLST(1,ICL)
        NZ = IPCLST(3,ICL)
        NM = IPCLST(2,ICL)
C-----HORIZONTAL CONDUCTANCES
        IF (PID.EQ.'HK  '.OR.PID.EQ.'HANI') THEN
          DO 70 I = 1, NROW
            DO 60 J = 1, NCOL
              DO 55 K=1,NLAY
                LT=LTHUF(K)
                IF (IBOUND(J,I,K).EQ.0) GOTO 55
                IND = J + NCOL*(I-1) + NRC*(K-1)
                HO = H(IND)
                R0 = ZERO
                RMLT0 = ONE
                IF (NZ.GT.0) THEN
                  RMLT0=0.
                  DO 30 JJ = 5,IPCLST(4,ICL)
                    IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ICL)) THEN
                      IF(NM.GT.0) THEN
                        RMLT0=RMLT(J,I,NM)
                      ELSE
                        RMLT0=1.
                      ENDIF
                    END IF
   30             CONTINUE
                ELSEIF(NM.GT.0) THEN
                  RMLT0=RMLT(J,I,NM)
                ENDIF

C
C Get thicknesses of hydrogeologic unit in this cell and adjacent cells
                TOP0=BOTM(J,I,LBOTM(K)-1)
                IF(LTHUF(K).NE.0.AND.H(IND).LT.TOP0) TOP0=H(IND)
                BOT0=BOTM(J,I,LBOTM(K))
                CALL SSEN1HUF1THK(TOP0,BOT0,
     1                          HUFTHK(J,I,NU,1),
     2                          HUFTHK(J,I,NU,2),TH0)
C-------CR
                IF (J.NE.NCOL) THEN
                  IF (PID.NE.'HANI'.AND.IBOUND(J+1,I,K).NE.0) THEN
                    TOP1C=BOTM(J+1,I,LBOTM(K)-1)
                    IF(LTHUF(K).NE.0.AND.H(IND+1).LT.TOP1C)
     &                  TOP1C=H(IND+1)
                    BOT1C=BOTM(J+1,I,LBOTM(K))
                    CALL SSEN1HUF1THK(TOP1C,BOT1C,
     1                          HUFTHK(J+1,I,NU,1),
     2                          HUFTHK(J+1,I,NU,2),TH1C)
                    IF(TH0.EQ.0..AND.TH1C.EQ.0.) GOTO 54
                    CALL SSEN1HUF1CH(CO,TH0,TH1C,HP,I,J,K,'CR',RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L)
                  IF (IUHFBP.GT.0 .AND. CO.NE.0.)
     &                CALL SSEN1HFB6MD(C,'CR',CO,DELC,DELR,HFBP,I,J,K,
     &                                 MXACTFB,NCOL,NHFB,NROW,
     &                                 TH0L,TH1L)
                    HH = HO - HP
                    R0 = R0 + CO*HH
                    RHS(J+1,I,K) = RHS(J+1,I,K) - CO*HH
                  ENDIF
                ENDIF
C-------CC
   54           IF (I.EQ.NROW) GOTO 50
                IF (IBOUND(J,I+1,K).EQ.0) GOTO 50
                  TOP1R=BOTM(J,I+1,LBOTM(K)-1)
                  IF(LTHUF(K).NE.0.AND.H(IND+NCOL).LT.TOP1R)
     &                TOP1R=H(IND+NCOL)
                  BOT1R=BOTM(J,I+1,LBOTM(K))
                  CALL SSEN1HUF1THK(TOP1R,BOT1R,
     1                          HUFTHK(J,I+1,NU,1),
     2                          HUFTHK(J,I+1,NU,2),TH1R)
                  IF(TH0.EQ.0..AND.TH1R.EQ.0.) GOTO 50
                IF (PID.EQ.'HK  ') THEN
                  CALL SSEN1HUF1CH(CO,TH0,TH1R,HP,I,J,K,'CC',RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L)
                ELSEIF(PID.EQ.'HANI') THEN
                  CALL SSEN1HUF1CHN(CO,TH0,TH1R,HP,I,J,K,RMLT0,
     &                          HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,
     &                          BOTM(1,1,LBOTM(K)),BOTM(1,1,LBOTM(K)-1),
     &                          NZ,NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,
     &                          TH0L,TH1L)
                ENDIF
                IF (IUHFBP.GT.0 .AND. CO.NE.0.)
     &              CALL SSEN1HFB6MD(C,'CC',CO,DELC,DELR,HFBP,I,J,K,
     &                               MXACTFB,NCOL,NHFB,NROW,TH0L,
     &                               TH1L)
                  IF (CO.EQ.ZERO) GOTO 50
                HH = HO - HP
                R0 = R0 + CO*HH
                RHS(J,I+1,K) = RHS(J,I+1,K) - CO*HH
   50           RHS(J,I,K) = RHS(J,I,K) + R0
   55         CONTINUE
   60       CONTINUE
   70     CONTINUE
        ENDIF
C--End Horizontal
C-------CV
        IF (PID.EQ.'VK'.OR.PID.EQ.'VANI'.OR.
     &      (PID.EQ.'HK'.AND.HGUVANI(NU).NE.0)) THEN
          DO 90 I = 1, NROW
            DO 80 J = 1, NCOL
              RMLT0=ONE
              IF (NZ.GT.0) THEN
                RMLT0=0.
                DO 31 JJ = 5,IPCLST(4,ICL)
                  IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ICL)) THEN
                    IF(NM.GT.0) THEN
                      RMLT0=RMLT(J,I,NM)
                    ELSE
                      RMLT0=1.
                    ENDIF
                  END IF
   31           CONTINUE
              ELSEIF(NM.GT.0) THEN
                RMLT0=RMLT(J,I,NM)
              ENDIF
              IF(RMLT0.EQ.0.) GOTO 80
              DO 85 K=1,NLAY-1
                IND = J + NCOL*(I-1) + NRC*(K-1)
                TOP1=BOTM(J,I,LBOTM(K)-1)
                IF(LTHUF(K).NE.0.AND.H(IND).LT.TOP1) TOP1=H(IND)
                BOT1=BOTM(J,I,LBOTM(K))
                TOP2=BOTM(J,I,LBOTM(K))
                IF(LTHUF(K+1).NE.0.AND.H(IND+NRC).LT.TOP2) 
     &              TOP2=H(IND+NRC)
                BOT2=BOTM(J,I,LBOTM(K)+1)
                MID1=0.5*(TOP1+BOT1)
                  MID2=0.5*(TOP2+BOT2)
                CALL SSEN1HUF1THK(MID1,MID2,
     1                  HUFTHK(J,I,NU,1),HUFTHK(J,I,NU,2),THK1)
                IF(THK1.EQ.0.) GOTO 85
                CALL SSEN1HUF1CV(PID,CO,THK1,IBP,IBM,NCOL,NROW,NLAY,CV,
     &                        DELR,DELC,J,I,K,IBOUND,IP,NU,NZ,IZON,
     &                        NZONAR,RMLT0,RMLT,NMLTAR,IOUT,HUFTHK,NHUF)
                IF (CO.EQ.ZERO) GOTO 85
                HH = ZERO
                IF (K.LT.NLAY .AND. IBP.NE.0) HH = H(IND) - H(IND+NRC)
                IF (K.LT.NLAY .AND. IBP.NE.0) THEN
                  RHS(J,I,K) = RHS(J,I,K) + CO*HH
                  RHS(J,I,K+1) = RHS(J,I,K+1) - CO*HH
                ENDIF
   85         CONTINUE
   80       CONTINUE
   90     CONTINUE
        ENDIF
C-----S
        IF (PID.EQ.'SS  ' .OR. PID.EQ.'SY  ') THEN
          IF (ISS.NE.0) GOTO 140
          DO 130 I = 1, NROW
            DO 120 J = 1, NCOL
              DO 110 K = 1, NLAY
                IF (IBOUND(J,I,K).LT.1) GOTO 120
                LT=LTHUF(K)
                IND = J + NCOL*(I-1) + NRC*(K-1)
                HO = H(IND)
                SHO = HOLD(IND)
                TP = BOTM(J,I,LBOTM(K)-1)
                IF (LT.NE.0) THEN
                  IF (PID.EQ.'SS  ' .AND. HO.LT.TP .AND. SHO.LT.TP)
     &              GOTO 110
                  IF (PID.EQ.'SY  ' .AND. HO.GE.TP .AND. SHO.GE.TP)
     &              GOTO 110
                ENDIF
                RMLT0=1.0
                IF (NZ.GT.0) THEN
                  RMLT0=0.
                  DO 32 JJ = 5,IPCLST(4,ICL)
                    IF(IZON(J,I,NZ).EQ.IPCLST(JJ,ICL)) THEN
                      IF(NM.GT.0) THEN
                        RMLT0=RMLT(J,I,NM)
                      ELSE
                        RMLT0=1.
                      ENDIF
                    END IF
   32             CONTINUE
                ELSEIF(NM.GT.0) THEN
                  RMLT0=RMLT(J,I,NM)
                ENDIF
                IF(RMLT0.EQ.0) GOTO 120
                TOP0=BOTM(J,I,LBOTM(K)-1)
                IF(LTHUF(K).NE.0.AND.HO.LT.TOP0) TOP0=HO
                BOT0=BOTM(J,I,LBOTM(K))
                CALL SSEN1HUF1THK(TOP0,BOT0,
     1                          HUFTHK(J,I,NU,1),
     2                          HUFTHK(J,I,NU,2),TH0)
                IF(ABS(TH0).LT.1E-6) GOTO 110
                IF (PID.EQ.'SS  ') THEN
                  CO = RMLT0*TH0*DELR(J)*DELC(I)/DELT
                ELSE
                  CO = RMLT0*DELR(J)*DELC(I)/DELT
                ENDIF
C        DAH AND DBH
                IF (LT.NE.0) THEN
                  IF (SHO.GE.TP .AND. HO.LT.TP) THEN
                    IF (PID.EQ.'SS  ') HO = TP
                    IF (PID.EQ.'SY  ') SHO = TP
                  ELSEIF (SHO.LT.TP .AND. HO.GE.TP) THEN
                    IF (PID.EQ.'SS  ') SHO = TP
                    IF (PID.EQ.'SY  ') HO = TP
                  ENDIF
                ENDIF
                RHS(J,I,K) = RHS(J,I,K) - CO*(SHO-HO)
  110         CONTINUE
  120       CONTINUE
  130     CONTINUE
        ENDIF
  140 CONTINUE
  150 CONTINUE
C
      RETURN
      END
C=======================================================================
      SUBROUTINE SEN1HUF1UN(ISS,DELT,NCOL,NROW,NLAY,SOLD,HNEW,
     &                  SNEW,DELR,DELC,IBOUND,RHS,SC1,CR,CC,KITER,
     &                  HK,HKCC,BOTM,NBOTM,HOLD,CV,HUFTHK,NHUF,IZON,
     &                  NZONAR,RMLT,NMLTAR)
C     ******************************************************************
C     COMPUTE SENSITIVITY-EQUATION RHS TERMS FOR UNCONFINED AQUIFERS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOTM, CC, CR, CV, DELC, DELR, DELT, HOLD, RHS, SC1, SCC,
     &     HK
      INTEGER I, IBOUND, J, K, KITER, KT, LT, NCOL, NLAY,
     &        NROW, NBOTM
      DOUBLE PRECISION HNEW(NCOL,NROW,NLAY), SNEW(NCOL,NROW,NLAY)
      DIMENSION HOLD(NCOL,NROW,NLAY), SOLD(NCOL,NROW,NLAY),
     &          DELR(NCOL), DELC(NROW), IBOUND(NCOL,NROW,NLAY),
     &          RHS(NCOL,NROW,NLAY), BOTM(NCOL,NROW,0:NBOTM), 
     &          CC(NCOL,NROW,NLAY), CR(NCOL,NROW,NLAY),
     &          SC1(NCOL,NROW,NLAY),HK(NCOL,NROW,NLAY),
     &          HKCC(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     &          HUFTHK(NCOL,NROW,NHUF,2), IZON(NCOL,NROW,NZONAR),
     &          RMLT(NCOL,NROW,NMLTAR)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
C
C-------TERMS FOR UNCONFINED AQUIFERS
      IF (KITER.GT.1) THEN
        DO 60 K = 1, NLAY
          LT = LTHUF(K)
          IF (LT.NE.0) THEN
            CALL SSEN1HUF1NL(HNEW,SNEW,NCOL,NROW,NLAY,HK,HKCC,DELR,DELC,
     &                     IBOUND,RHS,BOTM,NBOTM,CR,CC,K,LT)
          ENDIF
          IF (K.GT.1 .AND. LT.NE.0) THEN
            DO 50 I = 1, NROW
              DO 40 J = 1, NCOL
                IF (IBOUND(J,I,K).NE.0 .AND. IBOUND(J,I,K-1).NE.0
     &              .AND. HNEW(J,I,K).LT.BOTM(J,I,LBOTM(K)-1)) THEN
                  RHS(J,I,K-1) = RHS(J,I,K-1)+CV(J,I,K-1)*SNEW(J,I,K)
                  RHS(J,I,K) = RHS(J,I,K)-CV(J,I,K-1)*SNEW(J,I,K)
                ENDIF
   40         CONTINUE
   50       CONTINUE
          ENDIF
   60   CONTINUE
      ENDIF
C
C-------B MATRIX TIMES SOLUTION FROM LAST TIME STEP FOR SENSITIVITY-
C-------EQUATION SENSITIVITIES
      IF (ISS.EQ.0) THEN
        KT = 0
        DO 110 K = 1, NLAY
          IF (LTHUF(K).EQ.0) THEN
            DO 80 I = 1, NROW
              DO 70 J = 1, NCOL
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SC1(J,I,K)/DELT
   70         CONTINUE
   80       CONTINUE
          ELSE
            KT = KT + 1
            DO 100 I = 1, NROW
              DO 90 J = 1, NCOL
                IF(IBOUND(J,I,K).EQ.0) GOTO 90
                SCC = SC1(J,I,K)
                HO=HOLD(J,I,K)
                TOP=BOTM(J,I,LBOTM(K)-1)
                BOT=BOTM(J,I,LBOTM(K))
                IF (HO.LT.TOP)
     &              CALL SGWF1HUF1SC2(2,J,I,TOP,BOT,1.0D0,HO,1.0,DUM,
     &                    SCC,HUFTHK,NCOL,NROW,NHUF,IZON,NZONAR,RMLT,
     &                    NMLTAR,DELR(J)*DELC(I))
                RHS(J,I,K) = RHS(J,I,K) - SOLD(J,I,K)*SCC/DELT
   90         CONTINUE
  100       CONTINUE
          ENDIF
  110   CONTINUE
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1NL(H,A,NC,NR,NL,HK,HKCC,DELR,DELC,IBOUND,RHS,
     &                     BOTM,NBOTM,CR,CC,K,LT)
C-----VERSION 1000 01FEB1992
C     ******************************************************************
C     ADD NONLINEAR TERMS FOR SENSITIVITY EQUATION CALCULATIONS
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BO, BOTM, BP, CC, CO, CR, D1CC, D1CR, D2CC, D2CR, DELC, DELR,
     &     RHS, HK, TH1, TH2, TOP1, TOP2, ZERO
      INTEGER I, IBOUND, IND, J, K, LT, NC, NC1, NL, NR, NR1,
     &        NRC
      DOUBLE PRECISION A(NC*NR*NL), AO, AP, H(NC*NR*NL), HO, HP
      DIMENSION CR(NC,NR,NL), CC(NC,NR,NL), HK(NC,NR,NL),
     &          DELR(NC), DELC(NR), RHS(NC,NR,NL), IBOUND(NC,NR,NL), 
     &          BOTM(NC,NR,0:NBOTM),HKCC(NC,NR,NL)
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
C
      ZERO = 0.0
      NRC = NR*NC
      NR1 = NR - 1
      NC1 = NC - 1
C
C      CR
C
      IF (NC.GT.1) THEN
        DO 20 I = 1, NR
          DO 10 J = 1, NC1
            IF (IBOUND(J,I,K).EQ.0 .OR. IBOUND(J+1,I,K).EQ.0) GOTO 10
            IND = J + NC*(I-1) + NRC*(K-1)
            HO = H(IND)
            HP = H(IND+1)
            TOP1 = ZERO
            TOP2 = ZERO
            IF (LT.NE.0) THEN
              TOP1 = BOTM(J,I,LBOTM(K)-1)
              TOP2 = BOTM(J+1,I,LBOTM(K)-1)
              IF (TOP1.LT.HO .AND. TOP2.LT.HP) GOTO 10
            ENDIF
            AO = A(IND)
            AP = A(IND+1)
            BO = BOTM(J,I,LBOTM(K))
            BP = BOTM(J+1,I,LBOTM(K))
            TH1 = HO - BO
            TH2 = HP - BP
C-------MATRIX DERIVATIVES
C            D1CR = 0.0
C            D2CR = 0.0
            IF (TOP1.GT.HO)
     &        D1CR = (CR(J,I,K)**2)*DELR(J)/
     &               (DELC(I)*2.*HKCC(J,I,K)*(TH1**2))
            IF (TOP2.GT.HP)
     &        D2CR = (CR(J,I,K)**2)*DELR(J+1)/
     &               (DELC(I)*2.*HKCC(J+1,I,K)*(TH2**2))
C-------MULTIPLY BY SENSITIVITIES FROM LAST ITERATION
            CO = D1CR*AO + D2CR*AP
C-------MULTIPLY BY HEAD VECTOR AND ADD TO RHS
            RHS(J,I,K) = RHS(J,I,K) - CO*(HP-HO)
            RHS(J+1,I,K) = RHS(J+1,I,K) - CO*(HO-HP)
   10     CONTINUE
   20   CONTINUE
      ENDIF
C
C      CC
C
      IF (NR.GT.1) THEN
        DO 40 J = 1, NC
          DO 30 I = 1, NR1
            IF (IBOUND(J,I,K).EQ.0 .OR. IBOUND(J,I+1,K).EQ.0) GOTO 30
            IND = J + NC*(I-1) + NRC*(K-1)
            HO = H(IND)
            HP = H(IND+NC)
            TOP1 = ZERO
            TOP2 = ZERO
            IF (LT.NE.0) THEN
              TOP1 = BOTM(J,I,LBOTM(K)-1)
              TOP2 = BOTM(J,I+1,LBOTM(K)-1)
              IF (TOP1.LT.HO .AND. TOP2.LT.HP) GOTO 30
            ENDIF
            AO = A(IND)
            AP = A(IND+NC)
            BO = BOTM(J,I,LBOTM(K))
            BP = BOTM(J,I+1,LBOTM(K))
            TH1 = HO - BO
            TH2 = HP - BP
C-------MATRIX DERIVATIVES
            IF (TOP1.GT.HO)
     &          D1CC = (CC(J,I,K)**2)*DELC(I)/
     &                 (2.*DELR(J)*HK(J,I,K)*(TH1**2))
            IF (TOP2.GT.HP)
     &          D2CC = (CC(J,I,K)**2)*DELC(I+1)/
     &                 (2.*DELR(J)*HK(J,I+1,K)*(TH2**2))
C-------MULTIPLY BY DERIVATIVES FROM LAST ITERATION
            CO = ZERO
            CO = D1CC*AO + D2CC*AP
C-------MULTIPLY BY HEAD VECTOR AND ADD TO RHS
            RHS(J,I,K) = RHS(J,I,K) - CO*(HP-HO)
            RHS(J,I+1,K) = RHS(J,I+1,K) - CO*(HO-HP)
   30     CONTINUE
   40   CONTINUE
      ENDIF
      RETURN
      END
c======================================================================
      SUBROUTINE SSEN1HUF1THK(TOP,BOT,TOPU,THKU,THCK)
C
C     ******************************************************************
C     Determine contributing thicknesses of hydrogeologic units.
C     ******************************************************************
C
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
C      DOUBLE PRECISION HNEW
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C
C
      TOPL=TOP
      BOTU=TOPU-THKU
      IF(TOPU.LE.BOT.OR.BOTU.GE.TOPL) THEN
        THCK=0
      ELSE
        TOP1=TOPU
        BOT1=BOTU
        IF(TOPU.GT.TOPL) TOP1=TOPL
        IF(BOTU.LT.BOT) BOT1=BOT
        THCK=TOP1-BOT1
      ENDIF
      IF(ABS(THCK).LT.1E-4) THCK=0.
C
C4------RETURN
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1CH(CO,TH0,TH1,HP,I,J,K,CHAR,RMLT0,
     &                  HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,BOT,TOP,NZ,
     &                  NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV, FAC,
     &     RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1, TOP, U, V, ZERO
      INTEGER I, II, IJ, IND, J, K, IZON, NCOL, NLAY, NROW, NZ
      CHARACTER*2 CHAR
      DIMENSION HK(NCOL,NROW,NLAY), DELC(NROW), DELR(NCOL), 
     & BOT(NCOL,NROW), TOP(NCOL,NROW),HKCC(NCOL,NROW,NLAY),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR)
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP, H0
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      HP = ZERO
      CO = ZERO
      DR = DELR(J)
      DC = DELC(I)
      II = 0
      IJ = 0
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      IF (CHAR.EQ.'CR') IJ = 1
      IF (CHAR.EQ.'CC') II = 1
      RMLT1 = 1.
      IF (NZ.GT.0) THEN
        RMLT1=0.
        DO 30 JJ = 5,IPCLST(4,ICL)
          IF(IZON(J+IJ,I+II,NZ).EQ.IPCLST(JJ,ICL)) THEN
            IF(NM.GT.0) THEN
              RMLT1=RMLT(J+IJ,I+II,NM)
            ELSE
              RMLT1=1.
            ENDIF
          END IF
   30   CONTINUE
      ELSEIF(NM.GT.0) THEN
        RMLT1=RMLT(J+IJ,I+II,NM)
      ENDIF
      IF(RMLT0.EQ.0..AND.RMLT1.EQ.0.) RETURN
      IF (CHAR.EQ.'CR') THEN
        HP = H(IND+1)
        FAC = 2.*DC
        D0 = DR
        D1 = DELR(J+1)
        HK0=HK(J,I,K)
        HK1=HK(J+IJ,I+II,K)
      ELSEIF (CHAR.EQ.'CC') THEN
        HP = H(IND+NCOL)
        FAC = 2.*DR
        D0 = DC
        D1 = DELC(I+1)
        HK0=HKCC(J,I,K)
        HK1=HKCC(J+IJ,I+II,K)
      ENDIF
      H0=H(IND)
      TOP0L=TOP(J,I)
      TH0L=TOP0L-BOT(J,I)
      IF(LTHUF(K).NE.0.AND.H0.LT.TOP0L) TH0L=H0-BOT(J,I)
      TOP1L=TOP(J+IJ,I+II)
      TH1L=TOP1L-BOT(J+IJ,I+II)
      IF(LTHUF(K).NE.0.AND.HP.LT.TOP1L) TH1L=HP-BOT(J+IJ,I+II)
      T0 = HK0*TH0L
      T1 = HK1*TH1L
      DT0 = RMLT0*TH0
      DT1 = RMLT1*TH1
C U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C CONDUCTANCE TERM DIVIDED BY WHAT IS ALREADY IN FAC.
C DU AND DV ARE THEIR DERIVATIVES WITH RESPECT TO THE PARAMETER.
C UOV IS U DIVIDED BY V (U OVER V).
      U = T0*T1
      V = T0*D1 + T1*D0
      DU = T0*DT1 + T1*DT0
      DV = D1*DT0 + D0*DT1
      CO=0.          
C-----CHANGE VALUE TO MACHINE ZERO -- ASK STEVE
      IF(ABS(V).GT.1E-24) THEN
        C = FAC*U/V
        CO = FAC*(1./V**2)*(V*DU-U*DV)   
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1CHN(CO,TH0,TH1,HP,I,J,K,RMLT0,
     &                  HK,HKCC,NCOL,NROW,NLAY,DELC,DELR,H,BOT,TOP,NZ,
     &                  NM,ICL,IZON,NZONAR,RMLT,NMLTAR,C,TH0L,TH1L)
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE HORIZONTAL CONDUCTANCES WITH
C     RESPECT TO HORIZONTAL ANISOTROPY, FOR HARMONIC MEAN CONDUCTANCES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL BOT, CO, D0, D1, DC, DELC, DELR, DR, DT0, DT1, DU, DV, FAC,
     &     RMLT, RMLT0, RMLT1, HK, T0, T1, TH0, TH1, TOP, U, V, ZERO
      INTEGER I, IND, J, K, IZON, NCOL, NLAY, NROW, NZ
      DIMENSION DELC(NROW), DELR(NCOL), HK(NCOL,NROW,NLAY),
     & BOT(NCOL,NROW), TOP(NCOL,NROW),HKCC(NCOL,NROW,NLAY),
     & RMLT(NCOL,NROW,NMLTAR),IZON(NCOL,NROW,NZONAR)
      DOUBLE PRECISION H(NCOL*NROW*NLAY), HP, H0
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
      INCLUDE 'param.inc'
C     ------------------------------------------------------------------
      ZERO = 0.0
      HP = ZERO
      CO = ZERO
      DR = DELR(J)
      DC = DELC(I)
      IND = J + NCOL*(I-1) + NROW*NCOL*(K-1)
      RMLT1 = 1.
      IF (NZ.GT.0) THEN
        RMLT1=0.
        DO 30 JJ = 5,IPCLST(4,ICL)
          IF(IZON(J,I+1,NZ).EQ.IPCLST(JJ,ICL)) THEN
            IF(NM.GT.0) THEN
              RMLT1=RMLT(J,I+1,NM)
            ELSE
              RMLT1=1.
            ENDIF
          END IF
   30   CONTINUE
      ELSEIF(NM.GT.0) THEN
        RMLT1=RMLT(J,I+1,NM)
      ENDIF
      IF(RMLT0.EQ.0..AND.RMLT1.EQ.0.) RETURN
      H0=H(IND)
      HP = H(IND+NCOL)
      FAC = 2.*DR
      D0 = DC
      D1 = DELC(I+1)
      TOP0L=TOP(J,I)
      TH0L=TOP0L-BOT(J,I)
      IF(LTHUF(K).NE.0.AND.H0.LT.TOP0L) TH0L=H0-BOT(J,I)
      TOP1L=TOP(J,I+1)
      TH1L=TOP1L-BOT(J,I+1)
      IF(LTHUF(K).NE.0.AND.HP.LT.TOP1L) TH1L=HP-BOT(J,I+1)
      T0 = HKCC(J,I,K)*TH0L
      T1 = HKCC(J,I+1,K)*TH1L
      DT0 = HK(J,I,K)*RMLT0*TH0
      DT1 = HK(J,I+1,K)*RMLT1*TH1
C U AND V ARE THE NUMERATOR AND DENOMINATOR, RESPECTIVELY, OF THE
C CONDUCTANCE TERM DIVIDED BY WHAT IS ALREADY IN FAC.
C DU AND DV ARE THEIR DERIVATIVES WITH RESPECT TO THE PARAMETER.
C UOV IS U DIVIDED BY V (U OVER V).
      U = T0*T1
      V = T0*D1 + T1*D0
      DU = T0*DT1 + T1*DT0
      DV = D1*DT0 + D0*DT1
      CO=0.          
C-----CHANGE VALUE TO MACHINE ZERO -- ASK STEVE
      IF(ABS(V).GT.1E-24) THEN
        C = FAC*U/V
        CO = FAC*(1./V**2)*(V*DU-U*DV)   
      ENDIF
      RETURN
      END
C=======================================================================
      SUBROUTINE SSEN1HUF1CV(PID,CO,THK1,IBP,IBM,NCOL,NROW,NLAY,CV,
     &                    DELR,DELC,J,I,K,IBOUND,IP,NU,NZ,IZON,
     &                    NZONAR,RMLT0,RMLT,NMLTAR,IOUT,HUFTHK,NHUF)
C-----VERSION 1000 08AUG1995
C     VERSION 19980522 ERB
C     ******************************************************************
C     CALCULATE THE DERIVATIVE OF THE VERTICAL CONDUCTANCES WITH
C     RESPECT TO PARAMETER VALUES
C     ******************************************************************
C        SPECIFICATIONS:
C     ------------------------------------------------------------------
      REAL CO, CV, DELC, DELR, ZERO
      INTEGER I, IBM, IBOUND, IBP, J, K, IZON, NCOL, NLAY, NROW, NZ
      CHARACTER*4 PID
      DIMENSION DELC(NROW), DELR(NCOL), RMLT(NCOL,NROW,NMLTAR),
     &          IBOUND(NCOL,NROW,NLAY), CV(NCOL,NROW,NLAY),
     &          IZON(NCOL,NROW,NZONAR), HUFTMP(200),
     &          HUFTHK(NCOL,NROW,NHUF,2)
      INCLUDE 'param.inc'
      COMMON /DISCOM/LBOTM(200),LAYCBD(200)
      COMMON /HUFCOM/LTHUF(200),HGUHANI(200),HGUVANI(200),LAYWT(200)
C     ------------------------------------------------------------------
      ZERO = 0.0
      IBP = 0
      IBM = 0
      CO = ZERO
      IF(K.GT.1) IBM=IBOUND(J,I,K-1)
      IBP=IBOUND(J,I,K+1)
      IF (IBOUND(J,I,K).EQ.0 .OR. IBP.EQ.0) RETURN
      IF(PID.EQ.'VK') THEN
C---Vertical hydraulic conductivity
C-----First, get (additive) KV for this unit
        HUFTMP(NU)=0.0
        CALL SGWF1HUF1POP(HUFTMP,'VK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
        COD = DELR(J)*DELC(I)*(HUFTMP(NU))**2.
        CO = (CV(J,I,K)**2)*RMLT0*THK1/COD
      ELSE
C---Vertical Anisotropy
        IF(PID.EQ.'VANI') THEN
C-----First, get (additive) KH for this unit
        HUFTMP(NU)=0.0
        CALL SGWF1HUF1POP(HUFTMP,'HK  ',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
C          CALL SGWF1HUF1PSRCH('HK  ',NU,NZ,J,I,NROW,NCOL,IZON,
C     1                  NZONAR,BNP,RMLT,NMLTAR,RMLT1,IOUT)
          COD=DELR(J)*DELC(I)*(HUFTMP(NU)/(RMLT0*B(IP)))**2.
          CO=-(CV(J,I,K)**2)*THK1*RMLT0*(HUFTMP(NU)/
     &          (RMLT0*B(IP))**2)/COD
        ELSEIF(PID.EQ.'HK') THEN
C-----First, get VANI for this unit
        HUFTMP(NU)=0.0
        CALL SGWF1HUF1POP(HUFTMP,'VANI',NCOL,NROW,NHUF,I,J,HUFTHK,
     &                    IZON,NZONAR,RMLT,NMLTAR,NU,IOUT)
C          CALL SGWF1HUF1PSRCH('VANI',NU,NZ,J,I,NROW,NCOL,IZON,
C     1                  NZONAR,BNP,RMLT,NMLTAR,RMLT1,IOUT)
          COD = DELR(J)*DELC(I)*(RMLT0*B(IP)/(HUFTMP(NU)))**2.
          CO = ((CV(J,I,K)**2)*THK1*RMLT0/(HUFTMP(NU)))/COD
        ENDIF
      ENDIF

      RETURN
      END


