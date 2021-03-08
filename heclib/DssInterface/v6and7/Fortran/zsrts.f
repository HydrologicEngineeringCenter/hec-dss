      SUBROUTINE zsrts ( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,
     * CUNITS, CTYPE, IPLAN, ISTAT)
C
      implicit none
C
C     Store regular interval time series data short version
C     For data compression, quality flags or user header informaiton
C     use the extended version (zsrtsx6)
C
C
C
      INTEGER IFLTAB(*),zdssVersion
      CHARACTER CPATH*(*), CDATE*(*), CTIME*(*), CUNITS*(*), CTYPE*(*)
      REAL VALUES(*)
      INTEGER NVALS, IPLAN, ISTAT
C
      DOUBLE PRECISION DVALUES(1), COORDS(1)
      INTEGER QUALITY(1), IUHEAD(1)
      INTEGER ICDESC(1)
      INTEGER JCOMP, LDHIGH, NPREC
      REAL BASEV
      LOGICAL LBASEV
C
C
C
      JCOMP = 0
      NPREC = -1
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL zsrtsi6(IFLTAB, CPATH, CDATE, CTIME, NVALS,
     *    .false., VALUES, DVALUES, QUALITY, .false., CUNITS, CTYPE,
     *    IUHEAD, 0, COORDS, 0, ICDESC, 0,
     *    IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
      ELSE
          CALL zsrtsi7(IFLTAB, CPATH, CDATE, CTIME, NVALS,
     *    .false., VALUES, DVALUES, QUALITY, .false., CUNITS, CTYPE,
     *    IUHEAD, 0, COORDS, 0, ICDESC, 0,
     *    IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
      ENDIF
C
C
      RETURN
      END

