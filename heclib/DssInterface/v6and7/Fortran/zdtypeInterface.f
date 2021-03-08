      SUBROUTINE zdtype (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)
C
      implicit none
C
C     Determine the data type of a record and whether it exists
C
      INTEGER IFLTAB(*), zdssVersion
      CHARACTER CPATH*(*), CDTYPE*(*)
      LOGICAL LFOUND
      INTEGER NDATA, IDTYPE
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zdtype6 (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)
      ELSE
          CALL zdtype7 (IFLTAB, CPATH, NDATA, LFOUND, CDTYPE, IDTYPE)
      ENDIF
C
      RETURN
      END

