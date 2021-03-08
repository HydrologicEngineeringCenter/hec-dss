      SUBROUTINE zsetTimezone6 (itzOffset, tzName, iprecision)
C
C     Sets, or clears, the time zone, time zone offset and
C     data precision for time series data
C
C     Written by Bill Charley at HEC, 2016.
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssts.h'
C
C
      character tzname*(*)
      integer itzOffset, iprecision
C
C
      IWTZONE = itzOffset
C     The time zone identifier can contain up to 12 characters
C     Examples include 'PST', 'GMT+0800'
      CWTZONE = tzName
C
      IPREC = iprecision
C
      RETURN
      END

