      INTEGER FUNCTION isunitconnected (IUNIT)
C
C     Interface routine for functions to determine if a
C     FORTRAN unit number is currently being accessed
C
      INTEGER IUNIT
C
      LOGICAL LOPEN
      INTEGER ISTAT
C
      INQUIRE (UNIT=IUNIT, OPENED=LOPEN)
C
      IF (LOPEN) THEN
		ISTAT = 1
      ELSE
          ISTAT = 0
      ENDIF
C
      isunitconnected = ISTAT
C
      RETURN
      END

