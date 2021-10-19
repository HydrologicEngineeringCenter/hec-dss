      INTEGER FUNCTION fortranopen (IUNIT, FILENAME)
C
C     Interface routine for C/C++ functions to open a file
C     under FORTRAN
C
      INTEGER IUNIT
      CHARACTER FILENAME*(*)
      INTEGER length
C
      INTEGER ISTAT
C
#ifdef _MSC_VER
      OPEN (UNIT=IUNIT, FILE=FILENAME, SHARE="DENYNONE", IOSTAT=ISTAT)
#else
      OPEN (UNIT=IUNIT, FILE=FILENAME, IOSTAT=ISTAT)
#endif

      fortranopen = ISTAT
C
      RETURN
      END

