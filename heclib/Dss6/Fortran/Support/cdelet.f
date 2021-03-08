      SUBROUTINE CDELET (CNAME,IERR)
      implicit none
      integer IERR
      CHARACTER CNAME*(*)

      OPEN (UNIT=98, FILE=CNAME, IOSTAT=IERR)
      IF (IERR.EQ.0) CLOSE (UNIT=98, STATUS='DELETE')
C
      RETURN
      END

