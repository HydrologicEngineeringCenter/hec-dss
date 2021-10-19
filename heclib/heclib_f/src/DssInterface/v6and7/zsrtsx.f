      SUBROUTINE zsrtsx( IFLTAB, CPATH, CDATE, CTIME, NVALS, VALUES,
     * JQUAL, LQUAL, CUNITS, CTYPE, IUHEAD, NUHEAD, IPLAN,
     * JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
      implicit none
      integer nuhead,jcomp,nprec
      real basev
C
C     Z-Store Regular interval Time-Series data
C     Data is stored according to the
C     time window set from CDATE/CTIME and NVALS.
C
C     Input:
C        IFLTAB:  Working DSS array used in zopen6 call.  Must be
C                 be dimensioned as INTEGER with 1200 words
C        CPATH:   Pathname of the data to be stored.  The "D" part
C                 is ignored.  CPATH should be declared as
C                 CHARACTER*80.
C        CDATE:   Beginning date of the time window.  This may be
C                 a standard military style date (e.g. 01MAR74).
C        CTIME:   Beginning time of the time window.  This must be
C                 a standard 24 hour clock time (e.g. 1630).  Any
C                 time offset is implied by this date and time.
C                 CTIME should be declared as CHARACTER*4.
C        NVALS:   The number of data values to store.  This number
C                 defines the end of the time window.
C        VALUES:  The data to be stored.
C        CUNITS:  Character string containing the units of the data.
C                 CUNITS must be declared CHARACTER*8
C        CTYPE:   Character string containing the type of the data
C                 (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
C        IPLAN:   A flag indicating if existing data should be written
C                 over or not:
C                 IPLAN = 0  Always replace data.
C                 IPLAN = 1  Only replace missing data.
C                 IPLAN = 4  Do not allow a missing input data to
C                            replace a valid data piece.
C     Output:
C        ISTAT:   Integer status parameter, indicating the
C                 successfulness of the data storage.
C                 ISTAT = 0  All ok.
C                 ISTAT = 4  All missing data flags - no data stored
C                            unless IPLAN set to 2.
C                 ISTAT > 9  Illegal call to zsrts6
C
C     Written by Bill Charley at HEC, 1987.
C
C
C     DIMENSION STATEMENTS
C
C     Argument Dimensions
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*), CDATE*(*), CTIME*(*)
      INTEGER IFLTAB(*), IUHEAD(*), JQUAL(*)
      INTEGER IPLAN, ISTAT, NVALS
      LOGICAL LQUAL, LBASEV, LDHIGH
C
      REAL VALUES(*)
C
      DOUBLE PRECISION DVALUES(1), COORDS(1)
      INTEGER NCOORDS, ICDESC(1), NCDESC
C
      NCOORDS = 0
      NCDESC = 0
C
C
C
      CALL zsrtsi (IFLTAB, CPATH, CDATE, CTIME, NVALS,
     * .FALSE., VALUES, DVALUES, JQUAL, LQUAL, CUNITS, CTYPE,
     * IUHEAD, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC,
     * IPLAN, JCOMP, BASEV, LBASEV, LDHIGH, NPREC, ISTAT)
C
      RETURN
C
      END

