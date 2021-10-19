      LOGICAL FUNCTION ztstqp6 (QUALWD)
C
C
C     Test the protection bit on a DSS quality flag to see if on.
C
      INTEGER QUALWD
      INTEGER IBIT
C
      CALL BITS (QUALWD, 32, IBIT, .TRUE.)
      IF (IBIT.EQ.0) THEN
         ztstqp6 = .FALSE.
      ELSE
         ztstqp6 = .TRUE.
      ENDIF
C
      RETURN
      END

