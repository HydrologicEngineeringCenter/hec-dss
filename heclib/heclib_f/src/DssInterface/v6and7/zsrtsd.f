      SUBROUTINE zsrtsd( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,
     * CUNITS, CTYPE, IPLAN, ISTAT)
C
      implicit none
C
C     Store regular interval time series data short version
C     For data compression, quality flags or user header information
C     use the extended version (zsrtsx6)
C
C
C
      INTEGER IFLTAB(*),zdssVersion
      CHARACTER CPATH*(*), CDATE*(*), CTIME*(*), CUNITS*(*), CTYPE*(*)
      DOUBLE PRECISION VALUES(*)
      INTEGER NVALS, IPLAN, ISTAT
      INTEGER IGNORE1, IGNORE2, IGNORE3, IGNORE4, NPREC
C
      real vals(1), COORDS(1)
      INTEGER QUALITY(1), IUHEAD(1)
      INTEGER ICDESC(1)
C
C
C
      NPREC = -1
      IGNORE1 = 0;
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL zsrtsi6(IFLTAB, CPATH, CDATE, CTIME, NVALS,
     *    .true., vals, VALUES,QUALITY, .false., CUNITS, CTYPE,
     *    IUHEAD, 0, COORDS, 0, ICDESC, 0,
     *    IPLAN, IGNORE1, IGNORE2, IGNORE3, IGNORE4, NPREC, ISTAT)
      ELSE
          CALL zsrtsi7(IFLTAB, CPATH, CDATE, CTIME, NVALS,
     *     .true., vals, VALUES,QUALITY, .false., CUNITS, CTYPE,
     *    IUHEAD, 0, COORDS, 0, ICDESC, 0,
     *    IPLAN, IGNORE1, IGNORE2, IGNORE3, IGNORE4, NPREC, ISTAT)
      ENDIF
C
C
      RETURN
      END

