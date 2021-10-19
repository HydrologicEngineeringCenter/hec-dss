      INTEGER FUNCTION fortranclose (IUNIT)
C
C     Interface routine for C/C++ functions to close a file
C     open under FORTRAN
C
      INTEGER IUNIT
      INTEGER ISTAT
C
      CLOSE (UNIT=IUNIT, IOSTAT=ISTAT)
      fortranclose = ISTAT
C
      RETURN
      END

