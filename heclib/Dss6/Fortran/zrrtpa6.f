      SUBROUTINE zrrtpa6 (IFLTAB, CPATH, VALUES, NVALS, CUNITS,
     *                   CTYPE, IUHEAD, KHEADU, NUHEAD, ISTAT)

C
C
C
C     Retrieve regular interval time series pattern data,
C     time series data that has no specific date associated with it
C     such as a unit hydorgraph, or a cyclic data set, such as
C     daily average maximum temperatues
C
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*)
      INTEGER IFLTAB(*), IUHEAD(*)
      REAL VALUES(*)
      integer kvals, nvals, nheadi, headc, ipan, istat
C
      CHARACTER  CSCRAT*20
      LOGICAL LFOUND
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssmz.h'

      integer khead, nuhead, iplan, NHEADC, KHEADU
C
C     Developmental.
C     for now, just read the bloody thing in!
C     (Assume no time span, but this is the entire block!)
C
C     Set the dimension limit
      KVALS = NVALS
C
C     Read the data
      CALL zreadx6 (IFLTAB, CPATH, INTBUF, NIBUFF, NHEADI,
     *             HEADC, 0, NHEADC, IUHEAD, KHEADU, NUHEAD,
     *             VALUES, KVALS, NVALS, IPLAN, LFOUND)
C
C
      IF (LFOUND) THEN
         ISTAT = 0
C        Get the Header Information
         CALL HOLCHR (INTBUF(2),  1, 8, CSCRAT, 1)
         call strcpy(CUNITS, CSCRAT)
         CALL HOLCHR (INTBUF(4), 1, 8, CSCRAT,  1)
         call strcpy(CTYPE, CSCRAT)
      ELSE
         ISTAT = 5
      ENDIF
C
      RETURN
      END

