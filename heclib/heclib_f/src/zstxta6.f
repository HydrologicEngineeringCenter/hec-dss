      SUBROUTINE zstxta6 (IFLTAB, CPATH, CARRAY, NARRAY, IUHEAD,
     * NUHEAD, ISTAT)
C
C
C     Store text data passed as array CARRAY.
C
C     Written by Bill Charley at HEC, 1989
C
      INTEGER IFLTAB(*), IUHEAD(*), NARRAY, NUHEAD, ISTAT
      CHARACTER CPATH*(*)
      CHARACTER CARRAY(NARRAY)*(*)
C
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
      IF (MLEVEL.GE.7) WRITE (MUNIT, 20) NARRAY, NUHEAD, CPATH(1:NPATH)
 20   FORMAT (T5,'----- Enter zstxta6  -----',/,
     * T11,'Number of Lines in Array:',I4,'  User Header Length:',I5,/
     * T11,'Pathname: ',A)
C
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'ZXTXTA',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
      NBYTES = 0
      IPOS = 0
      NLINES = 0
 60   CONTINUE
      IF (NLINES.GE.NARRAY) GO TO 200
      NLINES = NLINES + 1
      CALL CHRLNB (CARRAY(NLINES), NLINE)
      IF (NLINE.EQ.0) NLINE = 1
C     Count the number of bytes to write (LF + NLINE + CR)
      NBYTES = IPOS + NLINE + 2
      IF (NBYTES.GT.MAXBYT) GO TO 910
      CALL CHRHOL (CHAR(10), 1, 1, ILBUFF, IPOS+1)
      CALL CHRHOL (CARRAY(NLINES), 1, NLINE, ILBUFF, IPOS+2)
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
 820  FORMAT(T5,'----- Exit zstxta6, Number of data values ',
     * 'stored:',I7,/,T11,'Number Bytes:',I6,',  Number Lines:',I5,
     * ',  Status:',I4)
C
      RETURN
C
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) MAXBYT, CPATH(1:NPATH)
 911  FORMAT (/,' *****DSS*** zstxta6:  ERROR - Insufficient Buffer',
     * ' Space to Store Text Data',/',  Maximum Number',
     * 'of Bytes:',I7,/,' Pathname: ',A,/)
      ISTAT = -3
      GO TO 800
C
      END
      SUBROUTINE zstxta(IFLTAB, CPATH, CARRAY, NARRAY, IUHEAD,
     * NUHEAD, ISTAT)
C
      INTEGER IFLTAB(*), IUHEAD(*), NARRAY, NUHEAD, ISTAT
      CHARACTER CPATH*(*)
      CHARACTER CARRAY(NARRAY)*(*)
C
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zstxta6 (IFLTAB, CPATH, CARRAY, NARRAY, IUHEAD,
     * NUHEAD, ISTAT)
      else
        call ztextStoreArray(IFLTAB, CPATH, CARRAY, NARRAY, IUHEAD,
     * NUHEAD, ISTAT)
      endif
      return
      end

