      SUBROUTINE zcatalogfile (IFLTAB, CFILE, ISTAT)
C
      implicit none
C
      INTEGER IFLTAB(*), zdssVersion
      CHARACTER CFILE*(*)
      INTEGER ISTAT
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zcatalogfile6 (IFLTAB, CFILE, ISTAT)
      ELSE
          CALL zcatalogfile7 (IFLTAB, CFILE, 0, 0, ISTAT)
      ENDIF
C
      RETURN
      END

