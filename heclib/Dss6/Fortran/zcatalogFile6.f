      SUBROUTINE zcatalogfile6 (IFLTAB, CFILE, ISTAT)
C
C
      IMPLICIT NONE
C
      INTEGER IFLTAB(*)
      CHARACTER CFILE*(*)
      INTEGER ISTAT, j, k
C
      INTEGER IFPOS, NPATH, ICOUNT
      CHARACTER CPATH*392, CINSTR*1
C
      OPEN (UNIT=123, FILE=CFILE, ERR=900)
C
      IFPOS = 0
      ICOUNT = 0
      CINSTR = ' '
      CPATH = ' '
 10   CONTINUE
        CALL zplist6 (IFLTAB, CINSTR, IFPOS, CPATH, NPATH, ISTAT)
        IF (ISTAT.LT.0) THEN
           CLOSE (UNIT=123)
           RETURN
        ENDIF
        IF (ISTAT.GT.0) GO TO 100
        ICOUNT = ICOUNT + 1
        j = icount/5000
        k = j *5000
        if (k.eq.icount) then
        write (*,*)'count = ',icount,',  path = ',CPATH(1:NPATH)
        endif
        WRITE (123, 20) CPATH(1:NPATH)
 20     FORMAT (A)
      GO TO 10
C
C
 100  CONTINUE
      CLOSE (UNIT=123)
      ISTAT = ICOUNT
      RETURN
C
 900  CONTINUE
      ISTAT = -2
      RETURN
      END

