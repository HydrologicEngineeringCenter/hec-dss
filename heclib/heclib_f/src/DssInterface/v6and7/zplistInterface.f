      SUBROUTINE zplist (IFLTAB, CINSTR, IFPOS, CPATH, NPATH, ISTAT)
C
      implicit none
C
      INTEGER IFLTAB(*)
      CHARACTER CINSTR*(*), CPATH*(*)
      INTEGER IFPOS, NPATH, ISTAT
      integer zdssVersion

      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zplist6(IFLTAB, CINSTR, IFPOS, CPATH, NPATH, ISTAT)
      ELSE
          CALL zplist7(IFLTAB, CINSTR, IFPOS, CPATH, NPATH, ISTAT)
      ENDIF
C
      RETURN
      END

