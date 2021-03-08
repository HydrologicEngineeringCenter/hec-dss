      SUBROUTINE zsrtpa6 (IFLTAB, CPATH, VALUES, NVALS, CUNITS, CTYPE,
     *                   IUHEAD, NUHEAD, IPLAN, ISTAT)
C
C
C
C     Store regular interval time series pattern data,
C     time series data that has no specific date associated with it
C     such as a unit hydorgraph, or a cyclic data set, such as
C     daily average maximum temperatues
C
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*)
      INTEGER IFLTAB(*), IUHEAD(*)
      REAL VALUES(*)
C
      CHARACTER  CSCRAT*20
      LOGICAL LFOUND
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssmz.h'
C
C     Developmental.
C     for now, just write the it out!
C     (Assume no time span, but this is the entire block!)
C
C
C     Add the Header Information
      NIHEAD = 5
      INTBUF(1) = IOFSET
      CSCRAT = CUNITS
      CALL CHRHOL (CSCRAT, 1, 8, INTBUF(2),  1)
      CSCRAT = CTYPE
      CALL CHRHOL (CSCRAT, 1, 8, INTBUF(4), 1)
C
C     Get the pathname length
      CALL CHRLNB (CPATH, NPATH)
C
      N = NUHEAD
      IF (N.LT.0) N = 0
C
C     Now store the data
      CALL zwritex6 (IFLTAB, CPATH, NPATH, INTBUF, NIHEAD,
     *             HEADC, 0, IUHEAD, N, VALUES, NVALS,
     *             101, IPLAN, ISTAT, LFOUND)
C
C
      RETURN
      END

