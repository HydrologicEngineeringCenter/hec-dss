      SUBROUTINE CHRFLB (CSTRING, IBEG, IEND)
C
C     CHRFLB finds the positions of the first and last non-blank
C     within a character string.  If the string contains all blanks,
C     both positions are returned as zero.
C
C     Variables:
C        CSTRING - Input character string
C        IBEG -    Position of the first non-blank character
C        IEND -    Position of the last non-blank character
C
      CHARACTER CSTRING*(*)
C
C
C     Find the last non-blank
      CALL CHRLNB(CSTRING, IEND)
C
      IF (IEND .EQ. 0) THEN
      IBEG= 0
      ELSE
      IBEG= NINDX(CSTRING,' ')
      END IF
C
      RETURN
      END

