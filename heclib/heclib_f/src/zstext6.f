      SUBROUTINE zstext6 (IFLTAB, CPATH, IUNIT, IUHEAD, NUHEAD,
     * NLINES, ISTAT)
C
C
C     Store text data
C     Reads the data from unit IUNIT
C
C     Written by Bill Charley
C
      INTEGER IFLTAB(*), IUHEAD(*)
      CHARACTER CPATH*(*)
      INTEGER IUNIT, NUHEAD, NLINES, ISTAT
C
      PARAMETER (KLINE=160)
      CHARACTER CLINE*(KLINE)
      LOGICAL LFOUND
C
      COMMON /WORDS/ IWORD(10)
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssts.h'
C
C
      ISTAT = 0
      MAXBYT = KLBUFF * IWORD(2)
      CALL CHRLNB (CPATH, NPATH)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) WRITE (MUNIT, 20) IUNIT, NUHEAD, CPATH(1:NPATH)
 20   FORMAT (T5,'----- Enter zstext6  -----',/,
     * T11,'Read from Unit:',I5,'  User Header Length:',I5,/
     * T11,'Pathname: ',A)
C
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zstext6',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
      NBYTES = 0
      IPOS = 0
      NLINES = 0
 60   CONTINUE
      READ (IUNIT, 80, END=200, ERR=900, IOSTAT=IST) CLINE
 80   FORMAT (A,A)
      CALL CHRLNB (CLINE, NLINE)
      IF (NLINE.EQ.0) NLINE = 1
      IF (CLINE(1:NLINE).EQ.CHAR(26)) GO TO 200
      NLINES = NLINES + 1
C     Count the number of bytes to write (LF + NLINE + CR)
      NBYTES = IPOS + NLINE + 2
      IF (NBYTES.GT.MAXBYT) GO TO 910
      CALL CHRHOL (CHAR(10), 1, 1, ILBUFF, IPOS+1)
      CALL CHRHOL (CLINE, 1, NLINE, ILBUFF, IPOS+2)
      CALL CHRHOL (CHAR(13), 1, 1, ILBUFF, IPOS+NLINE+2)
      IPOS = NBYTES
      GO TO 60
C
 200  CONTINUE
      NVALS = ((NBYTES - 1) / IWORD(2)) + 1
      JTYPE = 300
C
      CALL zwritex6 (IFLTAB, CPATH, NPATH, NBYTES, 1, ICHEAD, 0,
     * IUHEAD, NUHEAD, ILBUFF, NVALS, JTYPE, 0, ISTAT, LFOUND)
C
C
 800  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, NBYTES, NLINES, ISTAT
 820  FORMAT(T5,'----- Exit zstext6, Number of data values ',
     * 'stored:',I7,/,T11,'Number Bytes:',I6,',  Number Lines:',I5,
     * ',  Status:',I4)
C
      RETURN
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) IUNIT, IST, CPATH(1:NPATH)
 901  FORMAT (/,' *****DSS*** zstext6:  ERROR - Unable to Read from',
     * ' File.',/,' Reading from Unit:',I5,',  Error:',I5,/,
     * ' Pathname: ',A,/)
      ISTAT = -4
      GO TO 800
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) IUNIT, MAXBYT, CPATH(1:NPATH)
 911  FORMAT (/,' *****DSS*** zstext6:  ERROR - Insufficient Buffer',
     * ' Space to Store Text Data',/,' Unit:',I5,',  Maximum Number',
     * 'of Bytes:',I7,/,' Pathname: ',A,/)
      ISTAT = -3
      GO TO 800
C
      END
      SUBROUTINE zstext (IFLTAB, CPATH, IUNIT, IUHEAD, NUHEAD,
     * NLINES, ISTAT)
C
C
      INTEGER IFLTAB(*), IUHEAD(*)
      CHARACTER CPATH*(*)
      INTEGER IUNIT, NUHEAD, NLINES, ISTAT
C
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zstext6 (IFLTAB, CPATH, IUNIT, IUHEAD, NUHEAD,
     * NLINES, ISTAT)
      else
        call ztextStoreUnit(ifltab, cpath, IUNIT, IUHEAD,
     * NUHEAD, NLINES, ISTAT)
      endif
      return
      end

