      SUBROUTINE zirdow6 (ICUR, NPAIRS, NDOWN, IMULT, IBUFF)
C
C
C     For irregular interval time series data
C     Moves data in the array Buff down (or up) NDOWN spaces for
C     insertion (or deletion) of data
C
C     Written by Bill Charley
C
      INTEGER IBUFF(*)
C
C
C
      IF (NDOWN.GT.0) THEN
C
C     Enlarge the Buffer
      JCUR = ICUR + NDOWN
      DO 20 I=NPAIRS,JCUR,-1
      K = ((I-1) * IMULT) + 1
      J = ((I-NDOWN-1) * IMULT) + 1
      IF (IMULT.GE.4) IBUFF(K+3) = IBUFF(J+3)
      IF (IMULT.GE.3) IBUFF(K+2) = IBUFF(J+2)
      IBUFF(K+1) = IBUFF(J+1)
      IBUFF(K) = IBUFF(J)
 20   CONTINUE
C
      ELSE IF (NDOWN.LT.0) THEN
C
C     Shrink the buffer
      DO 40 I=ICUR,NPAIRS
      K = ((I-1) * IMULT) + 1
      J = ((I-NDOWN-1) * IMULT) + 1
      IBUFF(K) = IBUFF(J)
      IBUFF(K+1) = IBUFF(J+1)
      IF (IMULT.GE.3) IBUFF(K+2) = IBUFF(J+2)
      IF (IMULT.GE.4) IBUFF(K+3) = IBUFF(J+3)
 40   CONTINUE
C
      ENDIF
C
      RETURN
C
      END

