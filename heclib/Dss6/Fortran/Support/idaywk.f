      INTEGER FUNCTION IDAYWK (JUL)
C
C     RETURN INTEGER DAY OF WEEK GIVEN JULIAN DAY
C     WHERE JAN 1,1900 IS DAY 1
C     RETURN 1=SUNDAY, ETC.
C
      INTEGER*4 JUL                                                     ML
C
      IDAYWK=MOD(JUL,7)+1
C
      RETURN
      END

