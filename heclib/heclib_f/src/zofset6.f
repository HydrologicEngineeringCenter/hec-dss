      SUBROUTINE zofset6 (JULIAN, ITIME, INTERVAL, IFLAG, IOFSET)
C
C
C
C     Determine the time offset for regular interval time series data
C
C     JULIAN is the date, in hec julian days
C     ITIME is the time, in minutes past midnight
C     INTERVAL is the time interval, in minutes
C     IFLAG is a flag,
C     IFLAG:   0 - Only compute offset
C              1 - Compute offset, and adjust JUL and ITIME
C                  to the standard time
C              2 - Adjust JULIAN and ITIME according to IOFSET
C                  (after adjusting to standard, first)
C     IOFSET is the offset, in minutes
C
C     Re-written, April 1993 by Bill Charley, HEC
C        Added tri-monthly and semi-monthly intervals.
C
C
C
C
C     Clean up the time, if needed
      CALL DATCLN (JULIAN, ITIME, JUL, JTIME)
      JOFSET = -1
C
C     Compute the offset
      IF (INTERVAL.LE.1440) THEN
C        Daily interval, or less
         ITEMP = JTIME / INTERVAL
         JOFSET = JTIME - (ITEMP * INTERVAL)
C
      ELSE IF (INTERVAL.EQ.10080) THEN
C        Weekly interval
         KDAY = IDAYWK (JUL)
         JOFSET = ((KDAY - 1) * 1440) + JTIME
         IF (JOFSET.EQ.10080) JOFSET = 0
C
      ELSE
C        Compute the year, month and day, and the same
C        for one day later (to see if at the end of the month)
         IDUM = JLIYMD (JUL,   IYEAR, IMON, IDAY)
         ITEMP = JUL + 1
         IDUM = JLIYMD (ITEMP, JYEAR, JMON, JDAY)
C
         IF ((JDAY.EQ.1).AND.(JTIME.EQ.1440)) THEN
               JOFSET = 0
            ELSE
C
            IF (INTERVAL.EQ.14400) THEN
C              Tri-montly interval
C              Is this the rare case of a time just before the
C              end of the month?
               IF (IDAY.GE.30) IDAY = 9
               KDAY = MOD (IDAY, 10) - 1
               JOFSET = (KDAY * 1440) + JTIME
C
            ELSE IF (INTERVAL.EQ.21600) THEN
C              Semi-montly interval
C              Is this the rare case of a time just before the
C              end of the month?
               IF (IDAY.GE.30) IDAY = 14
               KDAY = MOD (IDAY, 15) - 1
               JOFSET = (KDAY * 1440) + JTIME
C
            ELSE IF (INTERVAL.EQ.43200) THEN
C              Montly interval
               KDAY = IDAY - 1
               JOFSET = (KDAY * 1440) + JTIME
               IF (JOFSET.GT.43200) JOFSET = 43200 - (1440 - JTIME)
            ENDIF
         ENDIF
C
         IF (INTERVAL.EQ.525600) THEN
C           Yearly interval
            IF ((JDAY.EQ.1).AND.(JMON.EQ.1).AND.(JTIME.EQ.1440)) THEN
               JOFSET = 0
            ELSE
               ITEMP = IYMDJL (IYEAR, 1, 1)
               JOFSET = ((JUL - ITEMP) * 1440) + JTIME
            ENDIF
         ENDIF
      ENDIF
C
C     If a non-standard interval, error out
      IF (JOFSET.EQ.-1) GO TO 900
C
C
C
C
C     Adjust the date/time to standard, if requested
      IF ((IFLAG.GE.1).AND.(JOFSET.NE.0)) THEN
         IF (INTERVAL.LE.10080) THEN
            ITEMP = JTIME + INTERVAL - JOFSET
            CALL DATCLL (JUL, ITEMP, JULIAN, ITIME)
C
         ELSE
C
C           Tri-monthly and greater
            JTIME = 2400
            IF (INTERVAL.EQ.14400) THEN
C              Tri-monthly
               IF (IDAY.LE.10) THEN
                  IDAY = 10
               ELSE IF (IDAY.LE.20) THEN
                  IDAY = 20
               ELSE
                  IDAY = 28
               ENDIF
C
            ELSE IF (INTERVAL.EQ.21600) THEN
C              Semi-monthly
               IF (IDAY.LE.15) THEN
                  IDAY = 15
               ELSE
                  IDAY = 28
               ENDIF
C
            ELSE
C              Monthly and yearly
               IDAY = 28
            ENDIF
C
            IF (INTERVAL.EQ.525600) IMON = 12
C
C           Now compute the standard date/time
            IF (IDAY.GE.25) THEN
               IMON = IMON + 1
               JULIAN = IYMDJL (IYEAR, IMON, 1) - 1
            ELSE
               JULIAN = IYMDJL (IYEAR, IMON, IDAY)
            ENDIF
            ITIME = 1440
         ENDIF
      ENDIF
C
C
C
C
C     Adjust date/time according to IOFSET, if requested
      IF ((IFLAG.EQ.2).AND.(IOFSET.NE.0)) THEN
         IF (IOFSET.GT.INTERVAL) GO TO 900
         IF (IOFSET.LT.0)        GO TO 900
C
C        Decrement the date/time by one period
         IDUM = INCTIM (INTERVAL, 0, -1, JULIAN, ITIME, JUL, JTIME)
C
C        Add the offset
         ITEMP = JTIME + IOFSET
C
C        Clean up the date/time
         CALL DATCLL (JUL, ITEMP, JULIAN, ITIME)
      ENDIF
C
C
C     Return the offset
      IF (IFLAG.NE.2) IOFSET = JOFSET
C
C
      RETURN
C
 900  CONTINUE
C     An error occurred (invalid input)
      IOFSET = -1
      RETURN
      END

