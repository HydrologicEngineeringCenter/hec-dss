      SUBROUTINE DATJUL (CDATE, JUL, IERR)
C
C     This subroutine takes a date, in a variety of styles,
C     and converts it into a julian date in days since Dec 31, 1899.
C     If no year is provided, the current year is assumed.
C     If no day is provided, the first of the month is assumed.
C
C     Valid style dates include:
C         March 21, 1982
C         21 MAR 82
C         21MAR82
C         March 21, 1882
C         March 82  (return julian date for March 21, 1982)
C         21 March  (return julian date for March 21 of the current year)
C         [Note: March 21 will return julian date for March 1, 1921, not
C          the 21st of March]
C         3/21/82  or  3-21-82
C
C     See the subroutine YMDDAT for a complete list
C
      COMMON /UNDEF_TIME/ ITIME_UNDEF
      INTEGER ITIME_UNDEF
C
      CHARACTER CDATE*(*)
      INTEGER*4 JUL, IYMDJL                                             MLu
C
C
C     CONVERT THE DATE TO YEAR, MONTH, DAY
      CALL DATYMD (CDATE, IYR, IMON, IDAY, IERR)
C
C     CONVERT THIS FORM INTO JULIAN
      IF (IERR.EQ.0) THEN
      JUL = IYMDJL ( IYR, IMON, IDAY)
      ELSE
      JUL = ITIME_UNDEF
      ENDIF
C
      RETURN
      END

