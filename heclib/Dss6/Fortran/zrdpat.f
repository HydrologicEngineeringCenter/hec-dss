      SUBROUTINE zrdpat(IUNIT, IPOS, INUMB, CTAG, CPATH, NPATH, LEND)
C
C
C    Read Pathnames according to reference numbers from the catalog file
C
C     Written by Bill Charley at HEC, 1990.
C
      CHARACTER CPATH*(*), CLINE*450, CFORMT*40, CTAG*(*), CPOS*7
      LOGICAL LEND, LTAG
      INTEGER IPLOC
C
      SAVE LTAG, CFORMT, IPLOC
C
C
      COMMON /ZDSSC1/ CPDATE, CPPROG
      CHARACTER CPDATE*7, CPPROG*6
C
C
C
      LEND = .FALSE.
C
      IF (IPOS.EQ.0) THEN
      REWIND IUNIT
C     Begin reading through file, looking for a pathname
      DO 80 I=1,20
      READ (IUNIT, 20, END=910) CLINE
 20   FORMAT (A)
C     Does this line contain a pathname ?
      IPLOC = INDEX (CLINE, '  /')
C
      IF (IPLOC.GT.0) THEN
C     Yes - Make a format to read it and other pathnames in the file
      IPLOC = IPLOC + 2
      IF (CLINE(6:6).EQ.' ') THEN
      LTAG = .FALSE.
      CPDATE = ' '
      CPPROG = ' '
      WRITE (CFORMT, 40) IPLOC
 40   FORMAT ('(A7,T',I2.2,',A)')
      ELSE
      LTAG = .TRUE.
      IF (IPLOC.LT.21) THEN
      WRITE (CFORMT, 50) IPLOC
 50   FORMAT ('(A7,1X,A8,T',I2.2,',A)')
      ELSE
      WRITE (CFORMT, 60) IPLOC
 60   FORMAT ('(A7,1X,A8,2X,A6,2X,A7,T',I2.2,',A)')
      ENDIF
      ENDIF
C
C     Now read the reference number, tag, and pathname
      IF (LTAG) THEN
         IF (IPLOC.LT.21) THEN
            READ (CLINE,CFORMT,ERR=920,IOSTAT=JERR) CPOS, CTAG, CPATH
         ELSE
      READ (CLINE,CFORMT,ERR=920,IOSTAT=JERR) CPOS, CTAG, CPPROG,
     * CPDATE, CPATH
         ENDIF
      ELSE
      READ (CLINE, CFORMT, ERR=920, IOSTAT=JERR) CPOS, CPATH
      call strcpy(CTAG, ' ')
      ENDIF
      READ (CPOS, *) IPOS
      IF (IPOS.LT.INUMB) GO TO 100
      GO TO 800
      ENDIF
C
 80   CONTINUE
      GO TO 910
      ENDIF
C
C
 100  CONTINUE
      IF (LTAG) THEN
         IF (IPLOC.LT.21) THEN
            READ (IUNIT, CFORMT, END=900, ERR=920, IOSTAT=JERR)
     *        CPOS, CTAG, CPATH
         ELSE
            READ (IUNIT, CFORMT, END=900, ERR=920, IOSTAT=JERR)
     *        CPOS, CTAG, CPPROG, CPDATE, CPATH
         ENDIF
      ELSE
         READ (IUNIT, CFORMT, END=900, ERR=920, IOSTAT=JERR)
     *     CPOS, CPATH
      call strcpy(CTAG, ' ')
      ENDIF
      READ (CPOS, *, IOSTAT=KERR) IPOS
      IF (KERR.NE.0) GO TO 800
      IF (IPOS.LT.INUMB) GO TO 100
C
C
 800  CONTINUE
      INUMB = IPOS
      CALL CHRLNB (CPATH, NPATH)
      IF (NPATH.LE.0) GO TO 910
C     Be sure this is a pathname
      IF (CPATH(1:1).NE.'/') GO TO 910
C     The following code takes care of a CR at the end of the line
C     for a nfs mount.
      IF (CPATH(NPATH:NPATH).NE.'/') THEN
         N = INDEXR(CPATH(1:NPATH), '/')
         IF (N.LE.0) GO TO 910
         CPATH(N+1:NPATH) = ' '
         NPATH = N
      ENDIF
C
 820  CONTINUE
      RETURN
C
C
C
C     We have reached the end of the catalog,
C     Set IPOS as a flag, and save the number of the last pathname
 900  CONTINUE
      INUMB = IPOS
      LEND = .TRUE.
      GO TO 820
C
 910  CONTINUE
      !WRITE (MUNIT,911)
 911  FORMAT (/' -----DSS--- zrdpat6:  Error - Unable to Recognize',
     *' the Catalog File ',/)
      GO TO 900
C
 920  CONTINUE
      !WRITE (MUNIT,921) JERR, CFORMT
 921  FORMAT (/' -----DSS--- zrdpat6:  Error during Read from',
     *' the Catalog File ',/' Error:',I5,',  Format: ',A,/)
      GO TO 900
C
      END

