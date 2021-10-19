      SUBROUTINE zupath (CPATH, IBPART, IEPART, ILPART, ISTAT)
      implicit none
C
C     This subroutine takes a pathname and determines the beginning
C     and ending positions of each of the pathname parts.
C
C     CPATH is the pathname character string.
C     IBPART is an array returning the beginning position of each part.
C     IEPART is an array returning the ending position of each part.
C     ILPART is an array returning the length of each part.
C     ISTAT is a parameter that is returned as -1 if an error
C           is found in the pathname.
C
C     Written by John Miwa at HEC, 1988.
C
      CHARACTER CPATH*(*)
      INTEGER IBPART(6), IEPART(6), ILPART(6), ISTAT
      integer NPATH,I,IPSL,J,K,ISLASH
C
C
      ISTAT = 0
      CALL CHRLNB (CPATH, NPATH)

C
      DO 10 I=1,6
      ILPART(I) = 0
 10   CONTINUE
C
C     Look for the first slash [/] in the pathname
      IPSL = INDEX(CPATH,'/')
      IF (IPSL.EQ.0) THEN
      ISTAT = -1
      GO TO 40
      ENDIF
C
      DO 20 I=1,6
      J = IPSL + 1
      IF (J.GT.NPATH) GO TO 900
      K = INDEX(CPATH(J:),'/')
C
C     Set flag if the pathname contains an error (not enough slashes)
      IF (K .EQ. 0) GO TO 900
C
      ISLASH = IPSL + K
C
C     Compute the length, beginning, and ending positions
      ILPART(I) = ISLASH - IPSL - 1
      IBPART(I) = IPSL + 1
      IEPART(I) = ISLASH - 1
      IF (ILPART(I).LE.0) IEPART(I) = IBPART(I)
C
C     Save the current slash position in the Previous Slash variable
      IPSL = ISLASH
 20   CONTINUE
C
 40   CONTINUE
      RETURN
C
 900  CONTINUE
      ISTAT = -1
      GO TO 40
C
      END

