      SUBROUTINE UPCASE_DISABLE ( CLINE)
C
C     ROUTINE TO CONVERT LOWER CASE CHARACTER TO UPPER CASE
C
      CHARACTER CLINE*(*)
C
      ILEN = LEN(CLINE)
C
C     LOOP TO TEST EACH CHARACTER IN THE LINE.  IF CHARACTER IS
C     IN LOWER CASE RANGE THEN CHANGE TO UPPER CASE.
C
      DO 10 I=1, ILEN
      JCH = ICHAR( CLINE(I:I) )
      IF ((JCH .GT. 96) .AND. (JCH .LT. 123))
     * CLINE(I:I) = CHAR(JCH-32)
 10   CONTINUE
C
      RETURN
      END

