      SUBROUTINE zrinfo (IFLTAB, CPATH, LFOUND, IDTYPE, CDTYPE, LDOUB,
     *                   LQUAL, IPRECIS, CRTAG, CLWDATE, CLWTIME,
     *                   CPNAME, IVERS, NDATA, NSPACE, ICOMPRES, LPASS)

C
      implicit none
C
C     Get record information
C     Written by Bill Charley at HEC, 2006.
C
C     Input:   IFLTAB  - DSS file table
C              CPATH   - Pathname
C     Output:  LFOUND  - Logical, true if exists
C              IDTYPE  - Integer, record type
C              CDTYPE  - Character, record type description (50 chr)
C              LDOUB   - Logical, double precision
C              LQUAL   - Logical, quality flags stored
C              IPRECIS - Integer, decimal precision of data
C              CRTAG   - Character, pathname tag (8 chr)
C              CLWDATE - Character, last written date (8 chr)
C              CLWTIME - Character, last written time (10 chr)
C              CPNAME  - Character, program name that wrote data (8)
C              IVERS   - Integer, version of data (number time written)
C              NDATA   - Integer, number of data
C              NSPACE  - Integer, space allocated for data
C              ICOMPRES- Integer, compress method (TS only, 0 for none)
C              LPASS   - Logical, password applied to this record
C
C
C
      INTEGER IFLTAB(*), IDTYPE, IPRECIS, IVERS, NDATA, NSPACE, ICOMPRES
      integer zdssVersion
      CHARACTER CPATH*(*)
      CHARACTER CDTYPE*(*), CRTAG*(*), CLWDATE*(*), CLWTIME*(*)
      CHARACTER CPNAME*(*)
      LOGICAL LFOUND, LDOUB, LQUAL, LPASS
      CHARACTER PATHNAME*393
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     FIX ME - MOVE PATHCHECK TO FUNCTION AND CALL ONLY
C     IF RECORD NOT FOUND!!!!
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Adjust the time interval for the DSS version, if necessary
      pathname = cpath
      call ztsPathCheckInterval(ifltab, PATHNAME)
C
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zrinfo6 (IFLTAB, PATHNAME, LFOUND, IDTYPE, CDTYPE, LDOUB,
     *                  LQUAL, IPRECIS, CRTAG, CLWDATE, CLWTIME,
     *                  CPNAME, IVERS, NDATA, NSPACE, ICOMPRES, LPASS)
      ELSE
          CALL zrinfo7 (IFLTAB, PATHNAME, LFOUND, IDTYPE, CDTYPE, LDOUB,
     *                  LQUAL, IPRECIS, CRTAG, CLWDATE, CLWTIME,
     *                  CPNAME, IVERS, NDATA, NSPACE, ICOMPRES, LPASS)
      ENDIF
C
      RETURN
      END

