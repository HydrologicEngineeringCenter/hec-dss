      SUBROUTINE znextts7 (IFLTAB, CPATH, CNEXT, LFORWARD, ISTAT)
C
C ***********************************************
C *******   Deprecated
C *******   Use ztsGetDateRange instead
C ***********************************************
c
      implicit none
      integer ISTAT
C
C     Given a time series pathname, determine
C     the next or previous pathname in time for this data set
C     (i.e., increment or decrement the Date part)
C
C     Written by Bill Charley at HEC, Sept. 1998.
C
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
      call strcpy(CNEXT, ' ')
C
C     Unform the pathname
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, IERR)
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
      CALL zgetinterval (INTL, CPATH(IBPART(5):IEPART(5)), NVALS, IERR)
      IF (IERR.EQ.0) THEN
C     Regular interval
C     FIX ME - INTERVAL IS IN SECONDS,
      INTL = INTL / 60
      CALL ZBEGDT7 (JUL, INTL, IYR, IMON, IDAY, IBLOCK)
      ELSE IF (IERR.EQ.1) THEN
C     Irregular interval
      CALL zirbeg7 (JUL, CPATH(IBPART(5):IEPART(5)), IYR,
     * IMON, IDAY, IBLOCK, IDUM1, IDUM2)
      ELSE
C     Not time series
      GO TO 900
      ENDIF
C
C     Increment the date to the next block
      IF (.NOT.LFORWARD) IBLOCK = -IBLOCK
      CALL zincbk7 (IBLOCK, JUL, IYR, IMON, IDAY)
C
C
C     Now we have the date of the next block - put it
C     back into the pathname
      call strcpy(CNEXT, CPATH)
      CALL JULDAT (JUL, 4, CNEXT(IBPART(4):IEPART(4)), IDUM1)
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

