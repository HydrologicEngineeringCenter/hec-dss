      SUBROUTINE zcatdr6 (CDREF, NDREF)
C
C
C     Catalog Date Reference
C     If the D (Date) part of a time-series pathname
C     is a reference relative to the current date, expand
C     it to reflect the proper date.
C     i.e., if D=M-2M, and today is 18MAR88, reset the
C     D part to D=01JAN1988.
C
      CHARACTER CDREF*(*)
C
C
      NDREF = 0
      CALL CHRLNB (CDREF, ILAST)
      IF (ILAST.GT.6) GO TO 800
C
C     Is the first character a valid reference? (Day/Month/Year)
      ITIME = INDEX ('DMY', CDREF(1:1))
      IF (CDREF(1:1).EQ.'T') ITIME = 1
      IF (ITIME.EQ.0) GO TO 800
C
C     Check that the second character is a plus or minus (+/-)
      IF (INDEX('+-',CDREF(2:2)).EQ.0) GO TO 800
C
C     Get the number of periods to increment
      I = ILAST - 2
      IF (I.LT.2) GO TO 800
      INC = INTCHR (CDREF(2:I+1))
      IF ((INC.EQ.-1).AND.(CDREF(2:I+1).NE.'-1')) GO TO 800
C
C     Get the the lenght of the increment (Day/Month/Year)
      JTIME = INDEX ('DMY', CDREF(ILAST:ILAST))
      IF (JTIME.EQ.0) GO TO 800
C
C     Get the current date
      CALL CURTIM (JUL, I)
C
C     Truncate the current date into the beginning of the month or yr
      IDUM = JLIYMD (JUL, IYR, IMON, IDAY)
      IF (ITIME.GE.2) THEN
      IDAY = 1
      IF (ITIME.EQ.3) IMON = 1
      ENDIF
      JUL = IYMDJL (IYR, IMON, IDAY)
C
C     Now increment this time by the specified increment
      IF (JTIME.EQ.1) THEN
C     By Days
      JUL = JUL + INC
      ELSE IF (JTIME.EQ.2) THEN
C     By Months
      JULS = JUL
      ILARGE = 30
      IDUM = INCTIM (ILARGE, 1, INC, JULS, 1440, JUL, JDUM)
      ELSE
C     By Years
      IYR = IYR + INC
      JUL = IYMDJL (IYR, IMON, IDAY)
      ENDIF
C
C     Convert our finished julian date back into a character date
      CALL JULDAT (JUL, 104, CDREF, NDREF)
C
 800  CONTINUE
      RETURN
C
      END
      SUBROUTINE zcatdr(CDREF, NDREF)
      character *(*) cdref
      call zcatdr6 (CDREF, NDREF)
      return
      end

