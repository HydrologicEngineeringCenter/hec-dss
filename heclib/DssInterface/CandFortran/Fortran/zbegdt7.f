      SUBROUTINE ZBEGDT7 (JUL, INTL, IYR, IMON, IDAY, IBLOCK, IVERNO)
C
C
C     For Regular interval time-series data, determine
C     the standard start date and block length, given the
C     time interval
C
C       JUL  - JULIAN DATE SINCE 31DEC1899
C       INTL - TIME INTERVAL IN MINUTES
C       JULB - DATE OF START OF BLOCK
C
      INTEGER JUL, INTL, IYR, IMON, IDAY, IBLOCK, IVERNO
C
      INTEGER KVALS(5)
      DATA KVALS /15, 60, 1440, 10080, 525600/
C
C
      IDUM = JLIYMD(JUL,IYR,IMON,IDAY)
      IF (INTL.LT.KVALS(1)) GO TO 100
      IF (INTL.LT.KVALS(2)) GO TO 150
      IF (INTL.LT.KVALS(3)) GO TO 200
      IF (INTL.LT.KVALS(4)) GO TO 300
      IF (INTL.LT.KVALS(5)) GO TO 400
      GO TO 500
C
C     USE DAILY BLOCK
100   IBLOCK=1
      GO TO 800
C
C     15 or 30 min data, use daily block for earlier
C     DSS versions, or monthly block for later versions
 150  CONTINUE
C     GO TO 200
C
C     USE MONTHLY BLOCK - BACKUP TO FIRST DAY OF MONTH
200   IDAY=1
      IBLOCK=2
      GO TO 800
C
C     USE YEARLY BLOCK - BACKUP TO FIRST DAY OF YEAR
300   IDAY=1
      IMON=1
      IBLOCK=3
      GO TO 800
C
C     USE DECADE BLOCK - BACKUP TO FIRST DAY OF DECADE
400   IDAY=1
      IMON=1
      IYR=(IYR/10)*10
      IBLOCK=4
      GO TO 800
C
C     USE CENTURY BLOCK - BACKUP TO FIRST DAY OF CENTURY
500   IDAY=1
      IMON=1
      IYR=(IYR/100)*100
      IBLOCK=5
C     GO TO 800
C
 800  CONTINUE
      RETURN
C
      END

