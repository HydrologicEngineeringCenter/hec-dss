      LOGICAL FUNCTION ztstqp7 (QUALWD)
C
      implicit none
C
C     Test the protection bit on a DSS quality flag to see if on.
C
      INTEGER QUALWD
      INTEGER IBIT
C
      CALL BITS (QUALWD, 32, IBIT, .TRUE.)
      IF (IBIT.EQ.0) THEN
         ztstqp7 = .FALSE.
      ELSE
         ztstqp7 = .TRUE.
      ENDIF
C
      RETURN
      END

