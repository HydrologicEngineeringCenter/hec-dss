      SUBROUTINE zmovwd6 (IFROM, ITO, NUMBER)
C
C     Move words from one array to another.
C     This is usually used to cheat when moving a real or
C     double array into an integer array, or visa versa.
C     NUMBER is the number of words to move
C
      INTEGER IFROM(*), ITO(*), NUMBER
C
      DO 10 I=1,NUMBER
          ITO(I) = IFROM(I)
 10   CONTINUE
C
      RETURN
      END

