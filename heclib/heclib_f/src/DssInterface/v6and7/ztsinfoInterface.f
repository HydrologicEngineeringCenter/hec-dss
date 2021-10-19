      SUBROUTINE ztsinfo (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     * CUNITS, CTYPE, LQUAL, LDOUBLE, LFOUND)
C
      implicit none
C
C     Retrieve information about a time series record
C     (both regular interval and irregular interval).
C     The pathname must be valid (with a correct D part).
C
C
C     Argument Dimensions
C
C     Argument Dimensions
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*)
      INTEGER IFLTAB(*),zdssVersion
      LOGICAL LQUAL, LDOUBLE, LFOUND
      INTEGER JULS, ISTIME, JULE, IETIME
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL ztsinfo6 (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     * CUNITS, CTYPE, LQUAL, LDOUBLE, LFOUND)
      ELSE
          CALL ztsinfo7 (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     * CUNITS, CTYPE, LQUAL, LDOUBLE, LFOUND)
      ENDIF
C
      RETURN
      END

