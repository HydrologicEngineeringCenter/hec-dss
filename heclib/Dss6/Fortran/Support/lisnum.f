      FUNCTION LISNUM (CSTRNG)
C
C     This logical function returns .TRUE. if CSTRNG contains
C     the ASCII representation of a number exclusively.
C
      LOGICAL LISNUM
      CHARACTER CSTRNG*(*)
C
      ILEN = LEN(CSTRNG)
      IVAL = NSCAN (CSTRNG,1,ILEN,' -+.0123456789Ee',1,16)
      IF (IVAL.EQ.0) THEN
      LISNUM = .TRUE.
      ELSE
      LISNUM = .FALSE.
      ENDIF
C
      RETURN
      END

