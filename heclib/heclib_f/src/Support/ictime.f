      INTEGER FUNCTION ICTIME (JUL1, ITIME1, JUL2, ITIME2)
C
C     Compares if one date/time pair is before, the same as, or
C     after another
C
C     INTEGER*6 ITIME1, ITIME2                                          H
      INTEGER*4 JUL1, ITIME1, JUL2, ITIME2, JL1, JL2                    ML
C
C
C
      IF ((ITIME1.GT.1440).OR.(ITIME1.LE.0)) THEN
      CALL DATCLL (JUL1, ITIME1, JL1, IT1)
      ELSE
      JL1 = JUL1
      IT1 = ITIME1
      ENDIF
C
      IF ((ITIME2.GT.1440).OR.(ITIME2.LE.0)) THEN
      CALL DATCLL (JUL2, ITIME2, JL2, IT2)
      ELSE
      JL2 = JUL2
      IT2 = ITIME2
      ENDIF
C
C
      IF (JL1.LT.JL2) THEN
      ICTIME = -1
      ELSE IF (JL1.EQ.JL2) THEN
      IF (IT1.LT.IT2) THEN
      ICTIME = -1
      ELSE IF (IT1.EQ.IT2) THEN
      ICTIME = 0
      ELSE
      ICTIME = 1
      ENDIF
      ELSE
      ICTIME = 1
      ENDIF
C
      RETURN
      END

