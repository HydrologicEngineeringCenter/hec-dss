      SUBROUTINE zincbk7 (IBLOCK, JUL, IYR, IMON, IDAY)
C
      implicit none
      INTEGER IYR, IMON, IDAY, IBLOCK,JBLOCK
      integer ISIGN,JUL,IYMDJL,K,JLIYMD
C
C     Increments to the next time block for
C     all time-series data
C     To decrement a block, make IBLOCK negative
C
C     Written by Bill Charley at HEC, 1989
C     Modified to decrement blocks, Oct, 1998
C
C
      JBLOCK = IABS (IBLOCK)
      IF (IBLOCK.LT.0) THEN
         ISIGN = -1
      ELSE
         ISIGN = 1
      ENDIF
C
      IF (JBLOCK.EQ.1) THEN
C     Increment by day
      JUL = IYMDJL(IYR,IMON,IDAY) + (ISIGN * 1)
      K = JLIYMD (JUL,IYR,IMON,IDAY)
C
      ELSE IF (JBLOCK.EQ.2) THEN
C     Increment by month
      IMON = IMON + (ISIGN * 1)
      IF (IMON.GT.12) THEN
      IYR = IYR + 1
      IMON = IMON - 12
      ELSE IF (IMON.LT.1) THEN
      IYR = IYR - 1
      IMON = IMON + 12
      ENDIF
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (JBLOCK.EQ.3) THEN
C     Increment by year
      IYR = IYR + (ISIGN * 1)
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (JBLOCK.EQ.4) THEN
C     Increment by decade
      IYR = IYR + (ISIGN * 10)
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (JBLOCK.EQ.5) THEN
C     Increment by century
      IYR = IYR + (ISIGN * 100)
      JUL = IYMDJL (IYR, IMON, IDAY)
      ENDIF
C
C
      RETURN
      END

