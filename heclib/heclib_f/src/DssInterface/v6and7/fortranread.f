      INTEGER FUNCTION fortranread (IUNIT, string, lenString, ISTAT)
C
C     Interface routine for C/C++ functions to write to a file
C     under FORTRAN
!      use ifcore
C
      INTEGER IUNIT, lenString
      CHARACTER string*(*)
C
      INTEGER ISTAT
C
      READ (IUNIT, 20, IOSTAT=ISTAT) STRING
 20   FORMAT(A)
      fortranread = ISTAT
      if (istat.eq.0) then
        call chrlnb(string, lenString)
      endif
C
      RETURN
      END

