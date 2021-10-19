      SUBROUTINE zloadcache6 (IFLTAB, ISTAT)
C
C
C     Read the entire file, which causes Windows to place it
C     in the system cache and make future IO operations fast.
C
C     Written by Bill Charley at HEC.
C
      INTEGER IFLTAB(*), ISTAT, ISIZE
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssmz.h'
C
      PARAMETER (KBUF=32768)
      INTEGER IBUF(KBUF), NBUF
C
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
      INTEGER NERROR, MAXERROR
      INTEGER(8) ISIZE8, IOFSET8, IPOS8, I8NUMB
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KHANDL)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zloadcache6;  Handle:',I5)
C
      IHANDL = IFLTAB(KHANDL)
      INTERRUPT = 0
      TOTAL_NUMB = 0
      CURRENT_NUMB = 0
C
      IOFSET8 = 0
      CALL seekf64 (IHANDL, 0, IOFSET8, IPOS8, ISTAT)
      CALL filesize64 (IHANDL, ISIZE8)
C
      NBUF = KBUF * 4
      CALL readf (IHANDL, IBUF, NBUF, ISTAT, NTRANS)
      IF (ISTAT.LT.0) GO TO 800
      IF (NTRANS.EQ.ISIZE8) GO TO 800
C
      NUMBREADS = ISIZE8/NBUF + 1
      TOTAL_NUMB = NUMBREADS
      DO 50 I=1,NUMBREADS
         CURRENT_NUMB = I
         I8NUMB = NTRANS
         IOFSET8 = IOFSET8 + I8NUMB
         CALL seekf64 (IHANDL, 0, IOFSET8, IPOS8, ISTAT)
         CALL readf (IHANDL, IBUF, NBUF, ISTAT, NTRANS)
         IF (ISTAT.LT.0) GO TO 800
         IF (INTERRUPT.NE.0) THEN
            ISTAT = 0
            GO TO 800
         ENDIF
 50   CONTINUE
C
 800  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT,810)
 810  FORMAT (T6,'-----DSS---Debug:  Exit  zloadcache6')
C
      RETURN
      END

