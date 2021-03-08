      SUBROUTINE ZRECADD(IFLTAB, CPATH, ILADD, ISTAT)
C
      implicit none
C
C     Returns internal addresses for a record
C
      INTEGER IFLTAB(*),zdssVersion
      CHARACTER CPATH*(*)
      INTEGER*8 ILADD(9)
      INTEGER ISTAT
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zrecadd6(IFLTAB, CPATH, ILADD, ISTAT)
      ELSE
          CALL zrecadd7(IFLTAB, CPATH, ILADD, ISTAT)
      ENDIF
C
      RETURN
      END

