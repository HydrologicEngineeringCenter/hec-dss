      SUBROUTINE zckmul ( IFLTAB)
C
      implicit none
C
C
      INTEGER IFLTAB(*),zdssVersion
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
         CALL zckmul6(IFLTAB)
      ELSE
         CALL zcheckMultiUser(IFLTAB)
      ENDIF
C
      RETURN
      END

