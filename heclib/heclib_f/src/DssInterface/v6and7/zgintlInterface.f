      SUBROUTINE zgintl (INTL, CHINTL, NODATA, ISTAT)
C
      implicit none
C
C     Get Interval for time series data
C
      INTEGER INTL, NODATA, ISTAT
      CHARACTER*(*) CHINTL
C
      !IF (IVERNO.ge.70000) THEN
          CALL zgintl6 (INTL, CHINTL, NODATA, ISTAT)
      !ELSE
          !CALL zgintl7 (INTL, CHINTL, NODATA, ISTAT)
      !ENDIF
C
      RETURN
      END

