      SUBROUTINE ztsends6(IFLTAB, CPATH, ISEARCH, JULS, ISTIME,
     * JULE, IETIME, LFOUND)
C
      INCLUDE 'zdssmz.h'
C
C     Get the dates and times of the end points of a time series.
C     The series may be either regular interval or irregular interval,
C     and the pathname given does not have to have a correct D part.
C
C     Input:
C        IFLTAB:  Working DSS array used in zopen6 call.
C        CPATH:   Pathname of one record in the data set.  The date
C                 part does not have to be set.
C        ISEARCH - The number of records to check beyond
C                 the last one found (typically 10), OR set to
C                 zero (0) to do an exhaustive search.
C
C     Output:
C        JULS:    The julian date of the first valid (non-missing)
C                 data value in the series.
C        ISTIME:  The time in minutes past midnight of this value
C        JULE:    The julian date of the last valid (non-missing)
C                 data value in the series.
C        IETIME:  The time in minutes past midnight of this value.
C        LFOUND:  Logical flag indicating if any records exist with
C                 these pathname parts.  If this is returned FALSE,
C                 all other output is undefined.
C
C     Written by Bill Charley
C
C
C     DIMENSION STATEMENTS
C
C     Argument Dimensions
      CHARACTER CPATH*(*)
      INTEGER IFLTAB(*)
      LOGICAL LFOUND
C
      CHARACTER CFIRST*392, CLAST*392, CNEXT*392
      CHARACTER CUNITS*8, CTYPE*8
      LOGICAL LQUAL, LDOUBLE
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 20) CPATH
 20   FORMAT(' Enter ztsend, path: ', A)
      CLAST = ' '
      CNEXT = ' '
      CFIRST = ' '

C     Get the first and last pathnames in this series
      CALL  ztsrange (IFLTAB, CPATH, ISEARCH, CFIRST, CLAST, NFOUND)
      IF (NFOUND.LE.0) THEN
         LFOUND = .FALSE.
         RETURN
      ENDIF
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 30) CFIRST, CLAST
 30   Format(' ztsend, first path: ', A,/,' last path: ',A)
C
C     Now get the dates and times of the end points of this series
      CALL ztsinfo (IFLTAB, CFIRST, JULS, ISTIME, JUL, ITIME, CUNITS,
     *              CTYPE, LQUAL, LDOUBLE, LFOUND)
C     Check for rare occurance of all missing data in a block
      IF (.NOT.LFOUND) THEN
         CALL CHRLNB(CFIRST, NFIRST)
         DO 100 I=1,200
            CALL znextts(IFLTAB, CFIRST, CNEXT, .TRUE., ISTAT)
            IF (ISTAT.NE.0) GO TO 800
            IF (CNEXT(1:NFIRST).EQ.CLAST(1:NFIRST)) GO TO 800
            CALL ztsinfo (IFLTAB, CNEXT, JULS, ISTIME, JUL, ITIME,
     *                    CUNITS, CTYPE, LQUAL, LDOUBLE, LFOUND)
            IF (LFOUND) GO TO 200
            CFIRST = CNEXT
 100     CONTINUE
         GO TO 800
      ENDIF
C
 200  CONTINUE
      CALL ztsinfo (IFLTAB, CLAST, JUL, ITIME, JULE, IETIME, CUNITS,
     *              CTYPE, LQUAL, LDOUBLE, LFOUND)
C
      IF (.NOT.LFOUND) THEN
         CALL CHRLNB(CLAST, NLAST)
         DO 300 I=1,200
            CALL znextts(IFLTAB, CLAST, CNEXT, .FALSE., ISTAT)
            IF (ISTAT.NE.0) GO TO 800
            IF (CNEXT(1:NLAST).EQ.CFIRST(1:NLAST)) GO TO 800
            CALL ztsinfo (IFLTAB, CNEXT, JUL, ITIME, JULE, IETIME,
     *                    CUNITS, CTYPE, LQUAL, LDOUBLE, LFOUND)
            IF (LFOUND) GO TO 800
            CLAST = CNEXT
 300     CONTINUE
         GO TO 800
      ENDIF
C
C     All Done!
C
 800  CONTINUE
      IF (MLEVEL.GE.10) WRITE (MUNIT, 30) JULS, JULE
 830  Format(' Exit ztsend, JULS: ', I8,' JULE: ',I8)

      RETURN
      END

