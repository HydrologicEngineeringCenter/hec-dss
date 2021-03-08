      SUBROUTINE zmin2r (IDATES, DATES, BDATE, NVALS)
C
C
C     Minutes to Real Date for irregular interval time series data
C
C     Converts a Date array in minutes past midnight of a base date
C     to fractions of a day since the base date
C     The compiler should show a type mismatch for IDATES
C
C     Written by Bill Charley, HEC,  Jan 1990
C
      INTEGER IDATES(*)
      REAL DATES(*)
C
C
      IF (NVALS.GT.0) THEN
      DO 20 I=1,NVALS
      DATES(I) = REAL(IDATES(I)) / 1440.0 + BDATE
 20   CONTINUE
      ENDIF
C
C
      RETURN
      END

