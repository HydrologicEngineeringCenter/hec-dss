      SUBROUTINE zrtxta6 (IFLTAB, CPATH, CARRAY, KARRAY, NLINES,
     * IUHEAD, KUHEAD, NUHEAD, ISTAT)
C
C
C     Retrieve text data and place into character array CARRAY.
C     CARRAY should be large enough to hold the text data
C     (Its dimension is KARRAY)
C     For writing text data to a unit (e.g., file) use zrtext6
C
C     Written by Bill Charley at HEC, 1990
C     Last modified, March 1995.  Added call to zrdbuf6
C
C
      INTEGER IFLTAB(*), IUHEAD(*),KARRAY
      integer ISTAT,NPATH,KUHEAD,JPOS,IPOS,NPOS,NLINES,NBYTES,N
      integer ICHEAD,NUHEAD,NBUFF,JTYPE,I,NREAD,NTOTAL,KPOS
      integer, PARAMETER :: KLINE=160 
      CHARACTER CARRAY(KARRAY)*(*)
      CHARACTER CPATH*(*), CLINE*(KLINE), CLINE2*(KLINE)
      LOGICAL LFOUND , LEND
      
      
C
      COMMON /WORDS/ IWORD(10)
      integer IWORD
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssts.h'
C
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zrtxta6',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
      ISTAT = 0
      CALL CHRLNB (CPATH, NPATH)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) WRITE (MUNIT, 20) KARRAY, KUHEAD,
     * CPATH(1:NPATH)
 20   FORMAT (T5,'----- Enter zrtxta6  -----',/,
     * T11,'Character Array Dimension:',I4,'  User Header Dimension:',
     * I5,/,T11,'Pathname: ',A)
C
C
      JPOS = 1
      IPOS = 0
      NPOS = 0
      NLINES = 0
C
C
      CALL zreadx6 (IFLTAB, CPATH, NBYTES, 1, N, ICHEAD, 0, N,
     * IUHEAD, KUHEAD, NUHEAD, ILBUFF, KLBUFF, NBUFF, 0, LFOUND)
C
      IF (.NOT.LFOUND) THEN
      ISTAT = -1
      GO TO 800
      ENDIF
C
      CALL zinqir6 (IFLTAB, 'TYPE', CLINE, JTYPE)
      IF (JTYPE.NE.300) GO TO 900
C
C
C     Do we need to call zrdbuf6 (not enough array space)?
      IF (NBUFF.GT.KLBUFF) THEN
         LEND = .FALSE.
      ELSE
         LEND = .TRUE.
      ENDIF
C
C
 80   CONTINUE
      IF (.NOT.LEND) THEN
         CALL zrdbuf6 (IFLTAB, CPATH, I, 0, N, ILBUFF, KLBUFF, NBUFF,
     *                LEND, 0, LFOUND)
      ENDIF
C
      NREAD = NBUFF * IWORD(2)
C
C
C     If the first character is a line feed, ignore it
      CALL HOLCHR (ILBUFF, 1, 1, CLINE, 1)
      IF (CLINE(1:1).EQ.CHAR(10)) JPOS = 2
C
 100  CONTINUE
      N = NBYTES - IPOS
      IF (N.GT.KLINE) N = KLINE
      NTOTAL = N + IPOS
      IF (NTOTAL.GT.NREAD) N = NREAD - IPOS
      IF (N.LE.0) THEN
         IF (LEND) THEN
            GO TO 800
         ELSE
            IPOS = 0
            JPOS = 1
            NBYTES = NBYTES - NREAD
            GO TO 80
         ENDIF
      ENDIF
      CALL HOLCHR (ILBUFF, IPOS+1, N, CLINE, 1)
      IPOS = IPOS + N
C
 120  CONTINUE
C     Look for a carriage return
      I = INDEX (CLINE(JPOS:N), CHAR(13))
C
      IF (I.EQ.0) THEN
      CLINE2 = CLINE(JPOS:)
      NPOS = N - JPOS + 1
      JPOS = 1
      GO TO 100
      ELSE
      KPOS = JPOS + I - 2
      ENDIF
C
      IF (NLINES.GE.KARRAY) THEN
      ISTAT = 1
      GO TO 800
      ENDIF
C
      NLINES = NLINES + 1
      IF (NPOS.EQ.0) THEN
      IF (KPOS.LT.JPOS) THEN
C     Write out a blank line
      CARRAY(NLINES) = ' '
      ELSE
      CARRAY(NLINES) = CLINE(JPOS:KPOS)
      ENDIF
C
      ELSE
      IF (KPOS.LT.JPOS) THEN
      CARRAY(NLINES) = CLINE2(1:NPOS)
      ELSE
      CARRAY(NLINES) = CLINE2(1:NPOS) // CLINE(JPOS:KPOS)
      ENDIF
      NPOS = 0
      ENDIF
C
      JPOS = KPOS + 3
      IF (JPOS.GT.KLINE) THEN
      JPOS = JPOS - KLINE
      GO TO 100
      ELSE
      IF (JPOS.GT.N) THEN
         IF (LEND) THEN
            GO TO 800
         ELSE
            IPOS = 0
            JPOS = 1
            NBYTES = NBYTES - NREAD
            GO TO 80
         ENDIF
      ENDIF
      GO TO 120
      ENDIF
C
 800  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NBUFF, NBYTES, NLINES, ISTAT
 820  FORMAT(T5,'----- Exit zrtxta6, Number of data values read:',I5,/,
     * T11,'Number Bytes:',I6,',  Number Lines:',I5,',  Status:',I5)
C
      RETURN
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) JTYPE, CPATH(1:NPATH)
 901  FORMAT (/,' *****DSS*** zrtxta6:  ERROR - Record Not Identified',
     * ' as TEXT data.',/,' Data Type:',I5,/,' Pathname: ',A)
      ISTAT = -2
      GO TO 800
C
C
      END

