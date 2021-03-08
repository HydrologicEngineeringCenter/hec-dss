      SUBROUTINE zcheck(IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      implicit none
C
C     Determine the data type of a record and whether it exists
C
      INTEGER IFLTAB(*),zdssVersion
      CHARACTER CPATH*(*)
      LOGICAL LFOUND
      INTEGER NPATH, NHEAD, NDATA
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zcheck6(IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
      ELSE
          CALL zcheck7(IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
      ENDIF
C
      RETURN
      END

