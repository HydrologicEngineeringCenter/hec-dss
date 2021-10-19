      SUBROUTINE zufpn (CA, NA, CB, NB, CC, NC, CD, ND, CE, NE, CF, NF,
     +                  CPATH, NPATH, ISTAT)
C
C
C     zufpn6 is a subroutine that takes a standard pathname and
C     segments it into six pathname parts.
C
C     CA, CB, CC, CD, CE, and CF are the character strings of the parts
C     NA, NB, NC, ND, NE, and NF are the lengths of the PN part strings
C     CPATH is the pathname character string
C     NPATH is the length of the pathname string
C     ISTAT is a status flag that is returned to the program
C
C     Written by John Miwa at HEC, 1988.
C
C
C     Declare variables
      CHARACTER CA*(*), CB*(*), CC*(*), CD*(*), CE*(*), CF*(*)
      CHARACTER CPATH*(*)
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C
C
      CALL CHRLNB (CPATH, ILEN)
      NP = MIN0 (ILEN, NPATH)
C
C     Get the positions of the pathname parts within the pathname string
      IF (NP.GT.0) THEN
      CALL zupath (CPATH(1:NP), IBPART, IEPART, ILPART, ISTAT)
      ELSE
      DO 20 I=1,6
      ILPART(I) = 0
 20   CONTINUE
      ENDIF
C
C     Assign the string lengths to the respective variables
      NA= ILPART(1)
      NB= ILPART(2)
      NC= ILPART(3)
      ND= ILPART(4)
      NE= ILPART(5)
      NF= ILPART(6)
C
C     Get each part string according to the values returned by zupath6
C     Assign the parts to the respective names to be returned
C
      IF (NA.GT.0) THEN
      CA = CPATH(IBPART(1):IEPART(1))
      ELSE
      CA = ' '
      ENDIF
C
      IF (NB.GT.0) THEN
      CB = CPATH(IBPART(2):IEPART(2))
      ELSE
      CB = ' '
      ENDIF
C
      IF (NC.GT.0) THEN
      CC = CPATH(IBPART(3):IEPART(3))
      ELSE
      CC = ' '
      ENDIF
C
      IF (ND.GT.0) THEN
      CD = CPATH(IBPART(4):IEPART(4))
      ELSE
      CD = ' '
      ENDIF
C
      IF (NE.GT.0) THEN
      CE = CPATH(IBPART(5):IEPART(5))
      ELSE
      CE = ' '
      ENDIF
C
      IF (NF.GT.0) THEN
      CF = CPATH(IBPART(6):IEPART(6))
      ELSE
      CF = ' '
      ENDIF
C
      RETURN
      END

