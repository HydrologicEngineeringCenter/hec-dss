      SUBROUTINE zrenam (IFLTAB, CPATHOLD, NPATHOLD, CPATHNEW,
     * NPATHNEW, LFOUND)
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
      CHARACTER CPATHOLD*(*), CPATHNEW*(*)
      INTEGER IFLTAB(*),zdssVersion
      LOGICAL LFOUND
      INTEGER NPATHOLD, NPATHNEW
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL zrenam6 (IFLTAB, CPATHOLD, NPATHOLD, CPATHNEW,
     *                 NPATHNEW, LFOUND)
      ELSE
         CALL zrenam7 (IFLTAB, CPATHOLD, NPATHOLD, CPATHNEW,
     *                 NPATHNEW, LFOUND)
      ENDIF
C
      RETURN
      END

