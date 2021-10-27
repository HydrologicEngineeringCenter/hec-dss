      SUBROUTINE zpirts (IFLTAB, CPATH, NPATH, IBUFF, KBUFF,
     * IDUM, NDUM, DATES, VALUES, NVALS, BDATE, CUNITS, CTYPE,
     * INFLAG, ISTAT)
C
C     Replaced by zsits6
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*), CUNITS*(*), CTYPE*(*)
      REAL DATES(*), VALUES(*)
C
      INCLUDE 'zdssmz.h'
C
C
      IBDATE = DATES(1) + BDATE
      JDATE = DATES(1)
C
C     Convert fractions of a day to minutes
      CALL zr2min (DATES, DATES, JDATE, NVALS)
C
C
      IFLAG = INFLAG
      CALL zsits (IFLTAB, CPATH(1:NPATH), DATES, VALUES, NVALS,
     * IBDATE, CUNITS, CTYPE, IFLAG, ISTAT)
C
C     Convert minutes back to fractions of a day
      CALL zmin2r (DATES, DATES, REAL(JDATE), NVALS)
C
C
 800  CONTINUE
      RETURN
C
      END

