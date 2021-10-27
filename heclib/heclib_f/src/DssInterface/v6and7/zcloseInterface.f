      SUBROUTINE zclose ( IFLTAB)
C
      implicit none
C
C
      INTEGER IFLTAB(*),zdssVersion
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL zclose6(IFLTAB)
      ELSE
         CALL zclose7(IFLTAB)
      ENDIF
C
      RETURN
      END

