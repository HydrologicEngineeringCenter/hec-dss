      SUBROUTINE RATTIM(CSTR, NSTR, RTIME, ISTAT)
c
c     Compute rating effective time from a string (pathname d-part)
c
c     The string must be of the form ddMMMyyyy[:hhmm].
c
c     Time returned is minutes since 31Dec1899:0000 (HecTime minutes)
c
c     Mike Perryman
c     Nov, 2003
c
      CHARACTER CSTR*(*), CDATE*80, CTIME*80
      INTEGER*4 NSTR, RTIME, ISTAT, IDX, IOFMIN, IOFHR

      CDATE = CSTR
      IDX = INDEX(CDATE(1:NSTR), ':')
      IF (IDX .GT. 0) THEN
         CTIME = CDATE(IDX+1:)
         CDATE(IDX:) = ' '
      END IF
      CALL DATJUL(CDATE, RTIME, ISTAT)
      IF (ISTAT .NE. 0) GO TO 9990
      IOFMIN = 0
      IF (IDX .GT. 0) THEN
         READ (CTIME, '(I4)', ERR=9990) IOFMIN
         IOFHR = IOFMIN / 100
         IOFMIN = IOFHR * 60 + MOD(IOFMIN, 100)
      END IF
      RTIME = RTIME * 1440 + IOFMIN
      ISTAT = 0
      GO TO 9999

 9990 ISTAT = -1

 9999 RETURN
      END

