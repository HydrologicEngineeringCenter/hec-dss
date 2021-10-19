      SUBROUTINE INT8TO4 (INT8, INT4)
C
C     Move integer*8 value bits into an integer*4 value,
C     without having to worry about sign problems
C
      INTEGER*4 INT4
      INTEGER*8 INT8
C
      INTEGER*8 I64
      INTEGER*4 I32(2)
      EQUIVALENCE (I64, I32)
C
      I32(1) = 1
      I64 = INT8
C     Next line is to try and keep optimizers from killing this function
      IF (I64.EQ.0) I32(1) = 0
      INT4 = I32(1)
      RETURN
      END

