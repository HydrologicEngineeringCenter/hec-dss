      SUBROUTINE zsetfi (IFLTAB, CPARAM, CALPHA, INUMB, ISTATUS)
C
      implicit none
C
C     Set a parameter for this file (non-global)
C
      INTEGER IFLTAB(*),zdssVersion
      CHARACTER CPARAM*(*), CALPHA*(*)
      INTEGER INUMB, ISTATUS
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zsetfi6 (IFLTAB, CPARAM, CALPHA, INUMB, ISTATUS)
      ELSE IF (IFLTAB(1).EQ.7) THEN
          CALL zsetfi7 (IFLTAB, CPARAM, CALPHA, INUMB, ISTATUS)
      ELSE
C        File not opened yet - need to call both sets
        CALL zsetfi6 (IFLTAB, CPARAM, CALPHA, INUMB, ISTATUS)
        CALL zsetfi7 (IFLTAB, CPARAM, CALPHA, INUMB, ISTATUS)
      ENDIF
C
      RETURN
      END

