      LOGICAL FUNCTION LCOMPAR (SVALUES, DVALUES, IPOS, LDOUBLE, VAL)
C
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*)
      LOGICAL LDOUBLE
      REAL VAL
      INTEGER IPOS
C
      IF (LDOUBLE) THEN
          IF (DVALUES(IPOS).EQ.DBLE(VAL)) THEN
             LCOMPAR = .TRUE.
          ELSE
              LCOMPAR = .FALSE.
          ENDIF
      ELSE
          IF (SVALUES(IPOS).EQ.VAL) THEN
             LCOMPAR = .TRUE.
          ELSE
              LCOMPAR = .FALSE.
          ENDIF
      ENDIF
C
      RETURN
      END

