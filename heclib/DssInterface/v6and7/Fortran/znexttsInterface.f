      SUBROUTINE znextts (IFLTAB, CPATH, CNEXT, LFORWARD, ISTAT)
C
      implicit none
C
C     Given a time series pathname, determine
C     the next or previous pathname in time for this data set
C     (i.e., increment or decrement the Date part)
C
C
      INTEGER IFLTAB(*), ISTAT,zdssVersion
      CHARACTER CPATH*(*), CNEXT*(*)
      LOGICAL LFORWARD
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL znextts6(IFLTAB, CPATH, CNEXT, LFORWARD, ISTAT)
      ELSE
          CALL znextts7(IFLTAB, CPATH, CNEXT, LFORWARD, ISTAT)
      ENDIF
C
      RETURN
      END

