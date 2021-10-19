      SUBROUTINE znextts6 (IFLTAB, CPATH, CNEXT, LFORWARD, ISTAT)
C
C
C     Given a time series pathname, determine
C     the next or previous pathname in time for this data set
C     (i.e., increment or decrement the Date part)
C
C     Written by Bill Charley at HEC, Sept. 1998.
C
      INCLUDE 'zdsskz.h'
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*), CNEXT*(*)
      LOGICAL LFORWARD
C
      INTEGER IERR, JUL, INTL, NVALS
      INTEGER IYR, IMON, IDAY, IBLOCK, IDUM1, IDUM2
      INTEGER IBPART(6), IEPART(6), ILPART(6)
C
C
C
C     Clear the next pathname
      CNEXT = ' '
C
C     Unform the pathname
      CALL zupath (CPATH, IBPART, IEPART, ILPART, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (ILPART(4).NE.9) GO TO 900
C
C     Get the julian date of this pathname
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)), JUL, IERR)
      IF (IERR.NE.0) GO TO 900
C
C     Get the julian date for the next block
C
C     Get the time interval
      IERR = 1
      CALL zgintl6 (INTL, CPATH(IBPART(5):IEPART(5)), NVALS, IERR)
      IF (IERR.EQ.0) THEN
C     Regular interval
      CALL zbegdt6 (JUL, INTL, IYR, IMON, IDAY, IBLOCK, IFLTAB(KVERNO))
      ELSE IF (IERR.EQ.1) THEN
C     Irregular interval
      CALL zirbeg6 (IFLTAB, JUL, CPATH(IBPART(5):IEPART(5)), IYR,
     * IMON, IDAY, IBLOCK, IDUM1, IDUM2)
      ELSE
C     Not time series
      GO TO 900
      ENDIF
C
C     Increment the date to the next block
      IF (.NOT.LFORWARD) IBLOCK = -IBLOCK
      CALL zincbk6 (IBLOCK, JUL, IYR, IMON, IDAY)
      if (iyr.le.200) go to 900
C
C
C     Now we have the date of the next block - put it
C     back into the pathname
      CNEXT = CPATH
      CALL JULDAT (JUL, 104, CNEXT(IBPART(4):IEPART(4)), IDUM1)
C
C     All done
      ISTAT = 0
      RETURN
C
C
 900  CONTINUE
      ISTAT = -1
      RETURN
      END

