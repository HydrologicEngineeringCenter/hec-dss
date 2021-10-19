      SUBROUTINE DATYMD (CDATE, IYR, IMON, IDAY, IERR)
C
C     This subroutine takes a date, in a variety of styles,
C     and converts it into an integer Year, Month, Day style date.
C     If no year is provided, the current year is assumed.
C     If no day is provided, the first of the month is assumed.
C
C     Valid style dates include:
C         March 21, 1982
C         21 MAR 82
C         21MAR82
C         March 21, 1882
C         March 82  (return julian date for March 21, 1982)
C         21 March  (return julian date for March 21 of the current year)
C         [Note: March 21 will return julian date for March 1, 1921, not
C          the 21st of March]
C         3/21/82  or  3-21-82
C
C     See the subroutine YMDDAT for a complete list
C
C
      CHARACTER CDATE*(*)
      CHARACTER CSTRNG*40, CMON(12)*3
      INTEGER*4 ILARGE                                                  MLu
C
      DATA CMON(01) /'JAN'/
      DATA CMON(02) /'FEB'/
      DATA CMON(03) /'MAR'/
      DATA CMON(04) /'APR'/
      DATA CMON(05) /'MAY'/
      DATA CMON(06) /'JUN'/
      DATA CMON(07) /'JUL'/
      DATA CMON(08) /'AUG'/
      DATA CMON(09) /'SEP'/
      DATA CMON(10) /'OCT'/
      DATA CMON(11) /'NOV'/
      DATA CMON(12) /'DEC'/
C
C
C     Determine the length of the input date, and transfer to CSTRNG
      CALL CHRLNB (CDATE, ILEN)
      IF ((ILEN.GT.40).OR.(ILEN.LT.3)) GO TO 900
      CSTRNG = CDATE(1:ILEN)
C
C     Convert any null or control characters to blanks
      DO 20 I=1,ILEN
      ICH = ICHAR(CSTRNG(I:I))
      IF (ICH.LT.32) CSTRNG(I:I) = ' '
 20   CONTINUE
      J = ILEN
      CALL CHRLNB (CSTRNG(1:J), ILEN)
C
C     Convert all characters to upper case
      CALL UPCASE (CSTRNG(1:ILEN))
C
C     Scan for a Month, Day, Year style date
C     (e.g. 3/24/82, or 3-24-82)
      ISLASH = ISCAN (CSTRNG, 1, ILEN, '/-', 1, 2, K)
      IF (ISLASH.GT.0) THEN
C
C     Dates in the Month, Day, Year style
      IF (ISLASH.EQ.1) GO TO 900
      IMON = INTGR (CSTRNG, 1, ISLASH-1, ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF ((IMON.LT.1).OR.(IMON.GT.12)) GO TO 900
C
      JSLASH = ISCAN (CSTRNG, ISLASH+1, ILEN-ISLASH, '/-', 1, 2, K)
      IF (JSLASH.EQ.0) JSLASH = ILEN + 1
      IDAY = INTGR (CSTRNG, ISLASH+1, JSLASH-ISLASH-1, ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF ((IDAY.LT.1).OR.(IDAY.GT.31)) GO TO 900
C
      IF (JSLASH.GE.ILEN) THEN
      CALL DATIME (IYR, IDUM1, ILARGE)
      ELSE
C
      IYR = INTGR (CSTRNG, JSLASH+1, ILEN-JSLASH, ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF (ILEN-JSLASH.EQ.2) CALL ADDCENTURY (IYR)
      ENDIF
C
      ELSE
C
C     Must be a normal style date (e.g. March 3, 1986)
C
C     For all style dates, the year is always the last piece
C     of information.  Read it first (then erase it)
C     Look backwards for the first non-digit.
      I = NSCAN (CSTRNG, ILEN, -ILEN, '0123456789', 1, 10)
      INUM = ILEN - I
C
      IF (INUM.EQ.0) THEN
C     If INUM = 0, No year given;  use current year
      CALL DATIME (IYR, IDUM1, ILARGE)
      ELSE
C
      IF ((I.EQ.0).OR.(INUM.LE.1).OR.(INUM.GT.4)) GO TO 900
      IYR = INTGR (CSTRNG, I+1, INUM, ISTAT)
      IF (ISTAT.NE.0) GO TO 900
C     If the length of the date is 2 digits, the current century is assumed
C     (Using this logic, the year 8 A.D. is represented by 0008)
      IF (INUM.LT.4) CALL ADDCENTURY (IYR)
C     Now erase the year (effectively done by resetting the length)
      ILEN = I
      ENDIF
C
C     Scan for the Month
      DO 120 J=1,12
      K = INDEX (CSTRNG(1:ILEN),CMON(J))
      IF (K.GT.0) THEN
      IMON = J
      GO TO 140
      ENDIF
 120  CONTINUE
C     No Month Found
      GO TO 900
C
 140  CONTINUE
C     Erase all non-numeric characters (this will leave us only the day)
      DO 160 I=1,ILEN
      ICH = ICHAR(CSTRNG(I:I))
      IF ((ICH.LT.48).OR.(ICH.GT.57)) CSTRNG(I:I) = ' '
 160  CONTINUE
C
C     Make sure this looks like a valid day
      CALL CHRFLB (CSTRNG(1:ILEN), IBEG, NUM)
      IEND = NUM - IBEG + 1
C
C     If no day is provided (NUM=0), use the first day of the month
      IF (NUM.EQ.0) THEN
      IDAY = 1
      ELSE
      IF (IEND.GT.2) GO TO 900
      IDAY = INTGR (CSTRNG, IBEG, IEND, ISTAT)
      IF (ISTAT.NE.0) GO TO 900
      IF ((IDAY.LT.1).OR.(IDAY.GT.31)) GO TO 900
      ENDIF
C
      ENDIF
C
      IERR = 0
      RETURN
C
C     Error Processing
 900  CONTINUE
      IERR = -1
      RETURN
      END

