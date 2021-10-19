      INTEGER FUNCTION fortranwrite2 (IUNIT, STRING1, STRING2)
C
C     Interface routine for C/C++ functions to write to a file
C     under FORTRAN
!      use ifcore
C
      INTEGER IUNIT
      CHARACTER STRING1*(*), STRING2*(*)
C
      INTEGER ISTAT
C
      WRITE (IUNIT, 20, IOSTAT=ISTAT) STRING1, STRING2
 20   FORMAT(A,A)
      fortranwrite2 = ISTAT
!      ISTAT = COMMITQQ(IUNIT)
C
      RETURN
      END

