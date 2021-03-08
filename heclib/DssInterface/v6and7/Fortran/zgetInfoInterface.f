      SUBROUTINE zgetinfo (IFLTAB, CPATH, IBUFF, ISTAT)
C
      implicit none
C
C
      INTEGER IFLTAB(*), IBUFF(*), ISTAT,zdssVersion
      CHARACTER CPATH*(*)
      CHARACTER PATHNAME*393
C
C
C     Adjust the time interval for the DSS version, if necessary
      pathname = cpath
      call ztsPathCheckInterval(ifltab, PATHNAME)
C
      IF (zdssVersion(IFLTAB).EQ.6) THEN
          CALL zgetinfo6(IFLTAB, PATHNAME, IBUFF, ISTAT)
      ELSE
          CALL zgetinfo7(IFLTAB, PATHNAME, IBUFF, ISTAT)
      ENDIF
C
      RETURN
      END

