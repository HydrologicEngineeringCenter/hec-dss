      SUBROUTINE zbegdt (JUL, INTL, IYR, IMON, IDAY, IBLOCK, IVERNO)
C
      implicit none
C     For Regular interval time-series data, determine
C     the standard start date and block length, given the
C     time interval
C
      INTEGER JUL, INTL, IYR, IMON, IDAY, IBLOCK, IVERNO
C
      IF (IVERNO.ge.70000) THEN
          CALL zbegdt7(JUL, INTL, IYR, IMON, IDAY, IBLOCK, IVERNO)
      ELSE
          CALL zbegdt6(JUL, INTL, IYR, IMON, IDAY, IBLOCK, IVERNO)
      ENDIF
C
      RETURN
      END

