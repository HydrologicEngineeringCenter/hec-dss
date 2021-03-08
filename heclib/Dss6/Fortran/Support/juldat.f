      SUBROUTINE JULDAT ( JUL, ISTYLE, CDATE, NDATE)
C
C     Converts an HEC style julian date (days since Dec 31, 1899),
C     Into a character date of various styles, as shown below
C
C     Input:
C        JUL:  The julian date, in days since Dec 31, 1899
C     Output:
C        CDATE:  A character variable to contain the date (should be
C                long enough to hold the date
C        NDATE:  The number of characters in CDATE
C
C
C  ISTYLE  Form   ISTYLE   Form      ISTYLE   Form      ISTYLE   Form
C      LC, 4 CH YR       LC, 2 CH YR     UC, 4 CH YR      UC, 2 CH YR
C  0:  June 2, 1985  10:  June 2, 85  100:  JUNE 2, 1985  110:  JUNE 2, 85
C  1:  Jun 2, 1985   11:  Jun 2, 85   101:  JUN 2, 1985   111:  JUN 2, 85
C  2:  2 June 1985   12:  2 June 85   102:  2 JUNE 1985   112:  2 JUNE 85
C  3:  June 1985     13:  June 85     103:  JUNE 1985     113:  JUNE 85
C  4:  02Jun1985     14:  02Jun85     104:  02JUN1985     114:  02JUN85
C  5:  2Jun1985      15:  2Jun85      105:  2JUN1985      115:  2JUN85
C  6:  Jun1985       16:  Jun85       106:  JUN1985       116:  JUN85
C  7:  02 Jun 1985   17:  02 Jun 85   107:  02 JUN 1985   117:  02 JUN 85
C  8:  2 Jun 1985    18:  2 Jun 85    108:  2 JUN 1985    118:  2 JUN 85
C  9:  Jun 1985      19:  Jun 85      109:  JUN 1985      119:  JUN 85
C
C     ISTYLE=-1:  CDATE = 6/2/85       ISTYLE=-11:  CDATE = 06/02/85
C     ISTYLE=-2:  CDATE = 6-2-85       ISTYLE=-12:  CDATE = 06-02-85
C
C     If ISTYLE is zero, it defaults to style 1.
C
C
      CHARACTER CDATE*(*)
      INTEGER*4 JUL                                                     MLu
C
      I = JLIYMD ( JUL, IYR, IMON, IDAY)
      IF (I.LT.0) THEN
         call strcpy(CDATE, ' ')
         NDATE = 1
         RETURN
      ENDIF
      CALL YMDDAT ( IYR, IMON, IDAY, ISTYLE, CDATE, NDATE, IERR)
	IF (IERR.LT.0) THEN
         call strcpy(CDATE, ' ')
         NDATE = 1
      ENDIF
C
      RETURN
      END

