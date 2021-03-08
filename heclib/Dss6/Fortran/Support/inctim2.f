      FUNCTION INCTIM2(INTERVAL, NPERIODS, JULS,
     *                 ISSECS, JULE, IESECS)
C
C
C     Increment Time:
C     Increments a date/time a given number of intervals
C     INTERVAL is the time interval, in minutes if intervalFlag is 0, or
C                                    in days    if intervalFlag is 1
C                                    in seconds,if intervalFlag is -1
C     NPERIODS is the number of periods to increment
C     JULS is the starting julian date (since Jan 1, 1990)
C     ISSECS is the starting time, in seconds past midnight
C     JULE is returned with the incremented ending julian date.
C     IESECS is returned with the ending time, in seconds past midnight.
C
C     For monthly, yearly, etc. intervals, the date/time is incremented
C     according to a logical interval.  For example, for a monthly
C     interval the date/time is increment by months, not 30 days
C     (e.g., Jan 31 to Feb 28 to Mar 31 to Apr 30).
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
      INTEGER*4 INTERVAL, JULS, JULE, IYMDJL                            ML
      INTEGER*4 INTL, I1440, NDAYS, NMINS, NPERYEAR, NLEFT              ML
      INTEGER*4 JYEAR, JMONTH, NPERIOD                                  ML
C     INTEGER*6 INTL, I1440, NDAYS, NMINS, NPERYEAR, NLEFT              H
C     INTEGER*6 JYEAR, JMONTH, NPERIOD                                  H
      LOGICAL LENDMO
C
C
C     These are to force Integer*4 math on DOS.
      I1440 = 1440
      NPERIOD = NPERIODS
C
C
      INTL = INTERVAL / 60
C
C     Is this a time interval that requires an end-of-period
C     calculation (e.g., last day of month, which can be
C     the 28th, 30th, 31st ...)?
      IF (INTL.EQ.14400) THEN
C        Tri-monthly
         NPERYEAR = 36
         NDAYPER = 10
      ELSE IF (INTL.EQ.21600) THEN
C        Semi-monthly
         NPERYEAR = 24
         NDAYPER = 15
      ELSE IF ((INTL.GE.40000).AND.(INTL.LT.45000)) THEN
C         Monthly
          NPERYEAR = 12
      ELSE IF ((INTL.GT.520000).AND.(INTL.LT.530000)) THEN
C         Yearly
          NPERYEAR = 1
      ELSE
C        Unknown.  Don't try to figure out end-of-period date
         NPERYEAR = 0
      ENDIF
C
C
C     Not one to worry about end-of-period.
      IF (NPERYEAR.EQ.0) THEN
C
C        If daily, its super simple
         IF ((intl.ge.1440).and.(MOD(INTL,I1440).EQ.0)) THEN
            JULE = JULS + ((INTL/I1440) * NPERIOD)
            IESECS = ISSECS
         ELSE if (INTERVAL.lt.60) THEN
C
C           Interval is in seconds
            NDAYS = (NPERIOD * INTERVAL) / (I1440 * 60)
            NSECS = (NPERIOD * INTERVAL) - (NDAYS*(I1440*60))
            JULE = JULS + NDAYS
            IESECS = ISSECS + NSECS


         ELSE
C
C        Compute the number of days and minutes, then add to beginning
            NDAYS = (NPERIOD * INTL) / I1440
            NMINS = (NPERIOD * INTL) - (NDAYS * I1440)
            JULE = JULS + NDAYS
            IESECS = ISSECS + (NMINS * 60)
C
         ENDIF
C
C        Clean up the time, if needed
        IF (IESECS.GT.(I1440 * 60)) THEN
            IESECS = IESECS - (I1440 * 60)
            JULE = JULE + 1
        ENDIF
        IF (IESECS.LT.0) THEN
            IESECS = IESECS + (I1440 * 60)
            JULE = JULE - 1
        ENDIF
C
C
      ELSE
C
C     This date requires an end-of-period date
C
         ISTIME = ISSECS / 60
         IETIME = ISTIME
         IDUM = JLIYMD (JULS, IYEAR, IMONTH, IDAY)
C
C        Is the starting time at the end of the month?
         JYEAR = JULS + 2
         IDUM = JLIYMD (JYEAR, IYR, IMON, IDY)
         IF (IMON.NE.IMONTH) THEN
            LENDMO = .TRUE.
            IOFSET = 0
         ELSE
            LENDMO = .FALSE.
C           Get any day offset
C           For example, the 4th would have an offset of 4
            IF (NPERYEAR.GE.24) THEN
               IOFSET = MOD (IDAY, NDAYPER)
            ENDIF
         ENDIF
C
C
C        Figure out the number of years to increment
         JYEAR = NPERIOD / NPERYEAR
         IYEAR = IYEAR + JYEAR
C
C        Figure out the number of months to increment
         IF (NPERYEAR.GE.12) THEN
            JMONTH = ((12 * NPERIOD) / NPERYEAR) - (JYEAR * 12)
            IMONTH = IMONTH + JMONTH
C
            IF (NPERYEAR.GT.12) THEN
               NLEFT = NPERIOD - ((JMONTH * NPERYEAR) / 12)
     *                 - (JYEAR * NPERYEAR)
               IF (NLEFT.LT.0) THEN
                  NLEFT = NLEFT + NPERYEAR/12
                  IMONTH = IMONTH - 1
               ENDIF
            ENDIF
         ENDIF
C
C        Clean up months
 100     CONTINUE
         IF (IMONTH.GT.12) THEN
            IYEAR = IYEAR + 1
            IMONTH = IMONTH - 12
            GO TO 100
         ELSE IF (IMONTH.LT.1) THEN
            IYEAR = IYEAR - 1
            IMONTH = IMONTH + 12
            GO TO 100
         ENDIF
C
C        Figure out day portion for monthly and yearly intervals
         IF (NPERYEAR.LE.12) THEN
            IF (LENDMO) THEN
               IMONTH = IMONTH + 1
               JULE = IYMDJL (IYEAR, IMONTH, 1) - 1
            ELSE
               JULE = IYMDJL (IYEAR, IMONTH, IDAY)
            ENDIF
C
         ELSE
C
C           Figure out day portion for semi and tri monthly
C           At this point, IDAY should be less than 32, and NLEFT
C           should be less than 3.
            IF (NLEFT.EQ.0) THEN
               IF (LENDMO) THEN
                  IMONTH = IMONTH + 1
                  JULE = IYMDJL (IYEAR, IMONTH, 1) - 1
               ELSE
                  JULE = IYMDJL (IYEAR, IMONTH, IDAY)
               ENDIF
            ELSE
               DO 30 I=1,NLEFT
                  NDAYMON = IYMDJL (IYEAR, IMONTH+1, 1) -
     *                      IYMDJL (IYEAR, IMONTH, 1)
                  IF (NDAYMON.GT.30) NDAYMON = 30
                  IDAY = IDAY + NDAYPER
                     IF (IDAY.GE.NDAYMON) THEN
C                    If near the end of the month, go to the last day
                        IMONTH = IMONTH + 1
                        JULE = IYMDJL (IYEAR, IMONTH, 1) -1 + IOFSET
C                       If one period beyond last day, add that period
                        IF ((IDAY.GT.32).AND.(IOFSET.EQ.0))
     *                      JULE = JULE + NDAYPER
                        IDUM = JLIYMD (JULE, IYEAR, IMONTH, IDAY)
                        IF ((IDAY.GE.NDAYMON).AND.(I.NE.NLEFT)) THEN
                           IDAY = 0
                           IMONTH = IMONTH + 1
                        ENDIF
                     ENDIF
 30            CONTINUE
            JULE = IYMDJL (IYEAR, IMONTH, IDAY)
            ENDIF
         ENDIF
C
C         Convert mins to seconds
        IESECS = IETIME * 60
C
      ENDIF
C
C     Be sure we are not at 0000 hours (but 2400 of the previous day)
      IF (IESECS.EQ.0) THEN
         IESECS = 1440*60
         JULE = JULE - 1
      ENDIF
C
      INCTIM2 = 0
C
      RETURN
      END

