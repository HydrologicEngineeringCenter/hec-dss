      SUBROUTINE zcofil (IFTOLD, IFTNEW, IARRAY, KARRAY, IBUFF, KBUFF,
     * LRCDEL, LRETAGX)
C
C
C
C     Converts from one DSS version to another...
C     Or just copies a file within the same version
C     What version the files are opened with determines
C     the conversion.
C
      INTEGER IFTNEW(*), IFTOLD(*), IBUFF(KBUFF)
      INTEGER IARRAY(KARRAY)
      LOGICAL LRCDEL, LRETAG, LRETAGX
C
      LRETAG = LRETAGX
      IF ((IFTNEW(1).EQ.6).and.(IFTOLD(1).EQ.6)) THEN
         CALL zcofil6(IFTOLD, IFTNEW, IARRAY, KARRAY, IBUFF, KBUFF,
     *   LRCDEL, LRETAG)
      ELSE IF ((IFTNEW(1).EQ.7).and.(IFTOLD(1).EQ.7)) THEN
C        A version 7 function
         CALL zcopyfile7(IFTOLD, IFTNEW, istat)
      ELSE
C       File not opened correctly
        istat = -1
      ENDIF
C
      RETURN
      END

