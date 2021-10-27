      SUBROUTINE YMDDAT ( IYR, IMON, IDAY, ISTYLE, CDATE, NDATE, IERR)
C
C     YMDDAT Converts a date in a Year, Month, Day integer form
C     into a Character date, availabe in several styles
C     Input:
C        IYR:  The integer year, either 2 or 4 digits
C        IMON: The integer month
C        IDAY: The integer day
C        ISTYLE:  The output style form, as shown below
C     Output:
C        CDATE:  A character variable to contain the date (should be
C                long enough to hold the date
C        NDATE:  The number of characters in CDATE
C        IERR:   Status flag, 0=OK, -1=ERROR.  Generally, the only
C                Errors occur from integer dates outside valid range.
C
C
C ISTYLE  Form   ISTYLE   Form      ISTYLE   Form      ISTYLE   Form
C     LC, 4 CH YR       LC, 2 CH YR     UC, 4 CH YR      UC, 2 CH YR
C  0  June 2, 1985  10  June 2, 85  100  JUNE 2, 1985  110  JUNE 2, 85
C  1  Jun 2, 1985   11  Jun 2, 85   101  JUN 2, 1985   111  JUN 2, 85
C  2  2 June 1985   12  2 June 85   102  2 JUNE 1985   112  2 JUNE 85
C  3  June 1985     13  June 85     103  JUNE 1985     113  JUNE 85
C  4  02Jun1985     14  02Jun85     104  02JUN1985     114  02JUN85
C  5  2Jun1985      15  2Jun85      105  2JUN1985      115  2JUN85
C  6  Jun1985       16  Jun85       106  JUN1985       116  JUN85
C  7  02 Jun 1985   17  02 Jun 85   107  02 JUN 1985   117  02 JUN 85
C  8  2 Jun 1985    18  2 Jun 85    108  2 JUN 1985    118  2 JUN 85
C  9  Jun 1985      19  Jun 85      109  JUN 1985      119  JUN 85
C
C     ISTYLE=-1:  CDATE = 6/2/85       ISTYLE=-11:  CDATE = 06/02/85
C     ISTYLE=-2:  CDATE = 6-2-85       ISTYLE=-12:  CDATE = 06-02-85
C
C
      CHARACTER CDATE*(*), CTDATE*20, CTEMP*6, CDAY*3, CMON*10, CYR*4
      CHARACTER CDELIM*1, CMONS(12)*10
      INTEGER NMONS(12)
      LOGICAL LCAPS, L4CHYR
C
      DATA CMONS(1)  /'January   '/, NMONS(1)  / 7/
      DATA CMONS(2)  /'February  '/, NMONS(2)  / 8/
      DATA CMONS(3)  /'March     '/, NMONS(3)  / 5/
      DATA CMONS(4)  /'April     '/, NMONS(4)  / 5/
      DATA CMONS(5)  /'May       '/, NMONS(5)  / 3/
      DATA CMONS(6)  /'June      '/, NMONS(6)  / 4/
      DATA CMONS(7)  /'July      '/, NMONS(7)  / 4/
      DATA CMONS(8)  /'August    '/, NMONS(8)  / 6/
      DATA CMONS(9)  /'September '/, NMONS(9)  / 9/
      DATA CMONS(10) /'October   '/, NMONS(10) / 7/
      DATA CMONS(11) /'November  '/, NMONS(11) / 8/
      DATA CMONS(12) /'December  '/, NMONS(12) / 8/
C
C
C
      IERR = 0
      CTDATE = ' '
      CTEMP = ' '
C
C     CHECK FOR ERRORS
      IF ((IMON.LT.1).OR.(IMON.GT.12)) GO TO 900
      IF ((IDAY.LT.1).OR.(IDAY.GT.31)) GO TO 900
      IF ((IYR.LT.0).OR.(IYR.GT.9999)) GO TO 900
C

      IF (ISTYLE.LT.0) THEN
      IF ((ISTYLE.EQ.-2).OR.(ISTYLE.EQ.-12)) THEN
      CDELIM = '-'
      ELSE
      CDELIM = '/'
      ENDIF
C
      JYR = IYR
      IF (JYR.GT.100) JYR = MOD (JYR, 100)
      IF (ISTYLE.LT.-10) THEN
      WRITE (CDAY(1:2),10) IDAY
      WRITE (CMON(1:2),10) IMON
      WRITE (CYR(1:2),10) JYR
 10   FORMAT (I2.2)
      ELSE
      WRITE (CDAY(1:2),20) IDAY
      WRITE (CMON(1:2),20) IMON
      WRITE (CYR(1:2),10) JYR
 20   FORMAT (I2)
      ENDIF
C
      CTDATE = CMON(1:2) // CDELIM // CDAY(1:2) // CDELIM // CYR(1:2)
      CALL REMBLK (CTDATE(1:8), CDATE, NDATE)
C
      ELSE
C
      JSTYLE = ISTYLE
      IF (JSTYLE.GE.100) THEN
      LCAPS = .TRUE.
      JSTYLE = JSTYLE - 100
      ELSE
      LCAPS = .FALSE.
      ENDIF
C
      IF (JSTYLE.GE.10) THEN
      L4CHYR = .FALSE.
      ELSE
      L4CHYR = .TRUE.
      ENDIF
C
      IBSTYL = MOD(JSTYLE,10)
C
C     GET MONTH
      CMON = CMONS(IMON)
      IF ((IBSTYL.GT.6).OR.(IBSTYL.EQ.1)) THEN
      NMON = 4
      CMON(4:4) = ' '
      ELSE IF (IBSTYL.LT.4) THEN
      NMON = NMONS(IMON) + 1
      ELSE
      NMON = 3
      ENDIF
C
C     GET YEAR
      JYR = IYR
      IF (JYR.LT.100) CALL ADDCENTURY (JYR)
      WRITE (CTEMP,'(I4.4)') JYR
      IF (L4CHYR) THEN
      CYR = CTEMP(1:4)
      NYR = 4
      ELSE
      CYR = CTEMP(3:4)
      NYR = 2
      ENDIF
C
C     GET DAY
      CDAY(1:3) = '   '
      K = MOD (IBSTYL,3)
      IF ((K.EQ.1).AND.(IBSTYL.GT.1)) THEN
      WRITE (CDAY(1:2),'(I2.2)') IDAY
      NDAY = 2
      ELSE IF ((K.EQ.2).OR.(IBSTYL.LE.1)) THEN
      IF (IDAY.GT.9) THEN
      WRITE (CDAY(1:2),'(I2)') IDAY
      NDAY = 2
      ELSE
      WRITE (CDAY(1:1), '(I1)') IDAY
      NDAY = 1
      ENDIF
      ELSE
      NDAY = 0
      ENDIF
C
C     SHOULD A BLANK FOLLOW THE DAY?
      IF ((IBSTYL.EQ.2).OR.(IBSTYL.EQ.7).OR.(IBSTYL.EQ.8)) NDAY=NDAY+1
C
C     NOW PUT TOGETHER DATE
      IF (IBSTYL.GT.1) THEN
      IPOS = 1
      IF (NDAY.GT.0) THEN
      CTDATE(IPOS:NDAY) = CDAY(1:NDAY)
      IPOS = IPOS + NDAY
      ENDIF
      CTDATE(IPOS:) = CMON(1:NMON) // CYR(1:NYR)
C
      ELSE
      CTDATE = CMON(1:NMON) // CDAY(1:NDAY) // ', ' // CYR(1:NYR)
      ENDIF
C
      IF (LCAPS) CALL UPCASE (CTDATE)
      CALL CHRLNB (CTDATE(1:20), NDATE)
      JLEN = LEN(CDATE)
      IF (NDATE.GT.JLEN) NDATE = JLEN
      call strcpy(CDATE(1:JLEN), CTDATE)
C
      ENDIF
C
      RETURN
C
C     ERROR
 900  CONTINUE
      call strcpy(CDATE, ' ')
      CDATE = ' '
      NDATE = 0
      IERR = -1
      RETURN
      END

