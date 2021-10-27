      INTEGER FUNCTION NOPERS2(INTERVAL, JULS, ISTIME, JULE, IETIME)
C
C
C     Number of Periods:
C     Determines the "number of periods" between two date/times
C     INTERVAL is the time interval, in seconds 
C     JULS is the starting julian date (since Jan 1, 1900)
C     ISTIME is the starting time, in seconds past midnight
C     JULE is ending julian date.
C     IETIME is the seconds time.
C     NOPERS2 is returned with the number of periods.
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
C     Written by Bill Charley, HEC
C        Updated April 1993
C
C
      INTEGER*4 INTERVAL, JULS, JULE                                    M
      INTEGER*4 INTL, NDAYS, NMINS                                      M
      INTEGER IFRACTION
C     INTEGER*6 INTL, NDAYS, NMINS                                      H
      REAL XPERS
C
C
C     If this a time interval that has irregual length
C     periods (e.g., monthly), get the year month and day.
C
      IF (INTERVAL.GT.840000) THEN
         INTL = INTERVAL/60
         IDUM = JLIYMD (JULS, ISYEAR, ISMONTH, ISDAY)
         IDUM = JLIYMD (JULE, IEYEAR, IEMONTH, IEDAY)      
C
C     Is it an interval with irregular length periods?
C
        IF (INTL.EQ.14400) THEN
C           Tri-monthly
         NOPERS2 = ((IEYEAR - ISYEAR) * 36) + ((IEMONTH - ISMONTH) * 3)
     *            + ((IEDAY - ISDAY) / 8)
        ELSE IF (INTL.EQ.21600) THEN
C        Semi-monthly
         NOPERS2 = ((IEYEAR - ISYEAR) * 24) + ((IEMONTH - ISMONTH) * 2)
     *            + ((IEDAY - ISDAY) / 13)
        ELSE IF ((INTL.GE.40000).AND.(INTL.LT.45000)) THEN
C         Monthly
         NOPERS2 = ((IEYEAR - ISYEAR) * 12) + (IEMONTH - ISMONTH)
     *            + ((IEDAY - ISDAY) / 27)
        ELSE IF ((INTL.GT.520000).AND.(INTL.LT.530000)) THEN
C         Yearly
         NOPERS2 = (IEYEAR - ISYEAR) + (((IEMONTH - ISMONTH)  +
     *            (IEDAY - ISDAY) / 28) / 12)
       endif
      ELSE
C
C        No - a straight forward calculation!
C        (Most intervals will use this)
         NDAYS = JULE - JULS
         NSECS = IETIME - ISTIME
         !  The following fails with large ndays
         !  NOPERS2 = ((NDAYS * 86400) + NSECS) / INTERVAL

         IF (INTERVAL.LE.86400) THEN
              !  Interval must be divisible with days
              !  compute number of periods per day
              IFRACTION = 86400 / INTERVAL   
              NOPERS2 = (NDAYS * IFRACTION) + (NSECS / INTERVAL)
         ELSE
            !  Use real fraction to compute number of periods
             FACT = 86400.0 / REAL(INTERVAL)
             XPERS = (REAL(NDAYS) * FACT) + (REAL(NSECS)/REAL(INTERVAL))
             IF (XPERS.GE.0.0) THEN
                XPERS = XPERS + 0.001
             ELSE 
                XPERS = XPERS - 0.001
             ENDIF
             NOPERS2 = INT(XPERS)
         ENDIF
C
      ENDIF
C
C
      RETURN
      END
