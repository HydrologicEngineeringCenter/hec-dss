      SUBROUTINE zneedsq (IFLTAB, ISTAT)
C
C     See if this file needs to be squeezed
C     If so, return istat = 1
C     If not, return istat = 0
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C     Written by Bill Charley
C
C
      INTEGER IFLTAB(*), ISTAT
C
      INTEGER IPOINT
      CHARACTER CSCRAT*20
      REAL FILSIZ, DEAD, DEADS, POINTU
C
C
      ISTAT = 0
C
C
      FILSIZ = REAL(IFLTAB(KFSIZE)) -1.
      FILSIZ = FILSIZ * (512./508.)
C     Compute the amount of dead space
      DEAD = FLOAT(IFLTAB(KDEAD))
      DEADS = (DEAD/FILSIZ) * 100.
      IF (DEADS.GT.10.0) ISTAT = 1
C
C     Compute a pointer effiency
      CSCRAT = ' '
      CALL zinqir6(IFLTAB, 'POINT', CSCRAT, IPOINT)
      POINTU = FLOAT(IPOINT) / 100.0
      IF (POINTU.GT.1.0) ISTAT = 1
C
      RETURN
      END

