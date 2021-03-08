      SUBROUTINE zdelet_old (IFLTAB, CPATH, NPATH, LFOUND)
C
      implicit none
C
C     Delete a record
C
      INTEGER IFLTAB(*),zdssVersion,NPATH
      CHARACTER CPATH*(*)
      LOGICAL LFOUND
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zdelet6 (IFLTAB, CPATH, NPATH, LFOUND)
      ELSE
          CALL zdelet7 (IFLTAB, CPATH, NPATH, LFOUND)
      ENDIF
C
      RETURN
      END

