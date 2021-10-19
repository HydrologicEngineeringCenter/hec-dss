      SUBROUTINE zirbeg (JUL, CE, IYR, IMON, IDAY, IBLOCK,
     * MINBLK, INCBLK)
C
      implicit none
C
C
      INTEGER JUL, IYR, IMON, IDAY, IBLOCK, MINBLK, INCBLK
      CHARACTER CE*(*)
C
      CALL zirbeg7(JUL, CE, IYR, IMON, IDAY, IBLOCK,
     * MINBLK, INCBLK)
C
      RETURN
      END

