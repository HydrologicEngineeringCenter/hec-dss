      INTEGER FUNCTION fortranwrite (IUNIT, STRING)
C
C     Interface routine for C/C++ functions to write to a file
C     under FORTRAN
!      use ifcore
C
      INTEGER IUNIT
      CHARACTER STRING*(*)
C
      INTEGER ISTAT
C
      WRITE (IUNIT, 20, IOSTAT=ISTAT) STRING
 20   FORMAT(A)
      fortranwrite = ISTAT
!      ISTAT = COMMITQQ(IUNIT)
C
      RETURN
      END

