      FUNCTION NOPERS (INTERVAL, MINDAYFLAG, JULS, ISTIME, JULE, IETIME)
C
C
C     Number of Periods:
C     Determines the "number of periods" between two date/times
C     INTERVAL is the time interval, in minutes if MINDAYFLAG is 0, or
C                                    in days    if MINDAYFLAG is 1.
C     JULS is the starting julian date (since Jan 1, 1900)
C     ISTIME is the starting time, in minutes past midnight
C     JULE is ending julian date.
C     IETIME is the ending time.
C     NOPERS is returned with the number of periods.
C
C     For monthly, yearly, etc. intervals, the irregular interval
C     length is accounted for.
C
C     Includes logic for "Semi-Monthly", and "Tri-Monthly" intervals.
C
C     Intervals have been tested for 1 minute to one year.
C     Intervals for greater than one day are:
C        Weekly:        10,080 minutes
C        Tri-Monthly:   14,400
C        Semi-Monthly:  21,600
C        Monthly:       43,200
C        Yearly:       525,600
C
C     Note INTERVAL, JULS, and JULE are INT*4 on MS-DOS
C
C     Written by Bill Charley, HEC
C        Updated April 1993
C
C
      INTEGER(4) INTERVAL, JULS, JULE                                    M
      INTEGER(8) INTL, NDAYS, NMINS, I8                                  M
C     INTEGER*6 INTL, NDAYS, NMINS                                      H
C
C
C
      IF (MINDAYFLAG.EQ.1) THEN
          INTL = INTERVAL * 1440
      ELSE
          INTL = INTERVAL
      ENDIF
C
C     If this a time interval that has irregual length
C     periods (e.g., monthly), get the year month and day.
C
      IF (INTL.GT.14000) THEN
         IDUM = JLIYMD (JULS, ISYEAR, ISMONTH, ISDAY)
         IDUM = JLIYMD (JULE, IEYEAR, IEMONTH, IEDAY)
      ENDIF
C
C     Is it an interval with irregular length periods?
C
      IF (INTL.EQ.14400) THEN
C        Tri-monthly
         NOPERS = ((IEYEAR - ISYEAR) * 36) + ((IEMONTH - ISMONTH) * 3)
     *            + ((IEDAY - ISDAY) / 8)
      ELSE IF (INTL.EQ.21600) THEN
C        Semi-monthly
         NOPERS = ((IEYEAR - ISYEAR) * 24) + ((IEMONTH - ISMONTH) * 2)
     *            + ((IEDAY - ISDAY) / 13)
      ELSE IF ((INTL.GE.40000).AND.(INTL.LT.45000)) THEN
C         Monthly
         NOPERS = ((IEYEAR - ISYEAR) * 12) + (IEMONTH - ISMONTH)
     *            + ((IEDAY - ISDAY) / 27)
      ELSE IF ((INTL.GT.520000).AND.(INTL.LT.530000)) THEN
C         Yearly
         NOPERS = (IEYEAR - ISYEAR) + (((IEMONTH - ISMONTH)  +
     *            (IEDAY - ISDAY) / 28) / 12)
      ELSE
C
C        No - a straight forward calculation!
C        (Most intervals will use this)
         NDAYS = JULE - JULS
         NMINS = IETIME - ISTIME
         I8 = NDAYS * 1440
         I8 = I8 + NMINS
         I8 = I8 / INTL
         NOPERS = I8
c        NOPERS = ((NDAYS * 1440) + NMINS) / INTL
C
      ENDIF
C
C
      RETURN
      END

