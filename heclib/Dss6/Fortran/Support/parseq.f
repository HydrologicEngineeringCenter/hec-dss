      SUBROUTINE PARSEQ (CLINE, MAXF, NFIELD, CLETTR, IBF, IEF, ILF)
C
C     Parses a line for equal signs, returning the beginning position,
C     ending position, and length of each field.
C     A field is delimited by a comma or a blank and a new field
C     (which is established by an equal sign).
C     The maximum number of fields (and diminsion limit
C     of the arrays are passed in as MAXF
C
C     Written by Bill Charley, HEC, September, 1989
C
C
      CHARACTER CLINE*(*), CLETTR(*)*(*)
      INTEGER IBF(*), IEF(*), ILF(*)
C
C
      CALL CHRLNB (CLINE, ILEN)
      NFIELD = 0
C     Too small a string?
      IF (ILEN.LT.3) GO TO 800
      IPOS = 2
C
 20   CONTINUE
      IF (NFIELD.GE.MAXF) GO TO 800
C     Parse for beginning of string (non-blank)
      J = ILEN - IPOS + 1
      N = ISCAN (CLINE, IPOS, J, '=', 1, 1, K)
      IF (N.EQ.0) GO TO 800
      NFIELD = NFIELD + 1
      IBF(NFIELD) = N + 1
      NPOS = N - 1
      IF ((CLINE(NPOS:NPOS).EQ.' ').AND.((N-2).GT.0)) NPOS = N - 2
      CLETTR(NFIELD) = CLINE(NPOS:NPOS)
      IF (NPOS.GT.1) THEN
      NPOS = NPOS - 1
      IF ((CLINE(NPOS:NPOS).NE.' ').AND.(CLINE(NPOS:NPOS).NE.','))
     * CLETTR(NFIELD) = ' '
      ENDIF
      IPOS = N + 1
C
      IF (IPOS.GT.ILEN) THEN
      IEF(NFIELD) = IBF(NFIELD)
      ILF(NFIELD) = 0
      GO TO 800
      ENDIF
C
C     Parse for end of string (blank or comma)
      J = ILEN - IPOS + 1
      N = ISCAN (CLINE, IPOS, J, ',=', 1, 2, K)
      IF (N.EQ.0) THEN
      IEF(NFIELD) = ILEN
      ILF(NFIELD) = IEF(NFIELD) - IBF(NFIELD) + 1
      ELSE
      IF (K.EQ.2) THEN
      J = N - 2
      N = ISCAN (CLINE, J, 2-N, ', ', 1, 2, K)
      ENDIF
      IEF(NFIELD) = N - 1
      ILF(NFIELD) = IEF(NFIELD) - IBF(NFIELD) + 1
      IPOS = N
      GO TO 20
      ENDIF
C
 800  CONTINUE
      RETURN
      END

