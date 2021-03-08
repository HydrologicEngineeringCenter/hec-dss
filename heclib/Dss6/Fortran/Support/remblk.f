      SUBROUTINE REMBLK ( CIN, COUT, NOUT)
C
C     Remove blank characters from CIN, placeing output in COUT
C     If CIN contains all blanks, COUT will be zero
C
      CHARACTER CIN*(*), COUT*(*)
C
C
      JIN = LEN(CIN)
      JOUT = LEN(COUT)
C
      NOUT = 0
      DO 20 I=1,JIN
      IF (CIN(I:I).NE.' ') THEN
      IF (NOUT.GE.JOUT) RETURN
      NOUT = NOUT + 1
      COUT(NOUT:NOUT) = CIN(I:I)
      ENDIF
 20   CONTINUE
C
      RETURN
      END

