      INTEGER(8) FUNCTION CONVI4TOI8 (I4)
C
C     Converts a 4 byte integer to a 8 byte integer without
C     messing up the sign bit
C
      INTEGER(4) I4
C
      INTEGER(4) IN4(2)
      INTEGER(8) INT8
      EQUIVALENCE (INT8, IN4(1))
C
      IN4(2) = 0
      IN4(1) = I4
      CONVI4TOI8 = INT8
C
      RETURN
      END

