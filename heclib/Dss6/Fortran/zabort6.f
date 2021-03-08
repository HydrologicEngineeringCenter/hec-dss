      SUBROUTINE zabort6 (IFLTAB, IERR, CSUB, JERR, IADD, CCOMET)
      
C
C     Records abort messages
C     On Harris records errors at the end of file SYST*DSS6-ERR
C
C     Written by Bill Charley at HEC, 1989
C
C     Error Codes:
C        10 - Pointer or address array incorrect
C        11 - Pointer or address array incorrect (Wrong Pathname)
C        20 - Illegal Unit number
C        30 - Error on Physical Read
C        40 - Error on Physical Write
C        41 - Disk Space Exceeded
C        50 - Corrupt IFLTAB array (Keys don't match)
C        60 - Incomplete Buffered write
C        70 - DSS File Not Opened
C       100 - Illegal KTABLE vaiable (IFLTAB Corrupt)
C       110 - Illegal Number of Characters per Machine word Set
C       120 - Time-series header length too small
C       130 - Excess write on read only file errors
C       200 - Unable to make shared assignment
C       210 - Unable to lock file
C       220 - Unalbe to unlock file
C       300 - Uncompatiable Versions
C       310 - Data Compression Used in old code
C       320 - Insufficient Header Space in zwrbuf6
C       330 - Insufficient Data Space in zwrbuf6
C
      INTEGER IFLTAB(*)
      CHARACTER CSUB*(*), CCOMET*(*)
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsslz.h'
C
C
C
C
      WRITE (MUNIT,20) IERR, CSUB
 20   FORMAT (' Error Code',I5,',  detected in function ',A)
      WRITE (MUNIT,22) IADD
 22   FORMAT (' Error detected at address ',I10)
      WRITE (MUNIT,23) CCOMET
 23   FORMAT (' Error: ',A)
      CALL CHRLNB (CKPATH, N)
      IF (N.EQ.0) N = 1
      WRITE (MUNIT, 30) CKPATH(1:N)
 30   FORMAT (' Last Pathname Accessed: ',A)
      WRITE (*,20) IERR, CSUB
      WRITE (*, 30) CKPATH(1:N)
C
C
C
      IF (LNOABT) THEN
         IF (IERRMS.EQ.0) THEN
            IERRMS = IERR
            WRITE(CERRMS, 140)IERR, CCOMET, CSUB
 140        FORMAT ("Error:",I4,"; ",A,", detected in ",A)
         ENDIF
         IF (IERR.NE.11) THEN
            WRITE (MUNIT, 120)
 120        FORMAT (/,' ----DSS:  File place in read access mode only.')
            IFLTAB(KREADO) = 1
            IFLTAB(KSTAT) = IERR
         ENDIF
         RETURN
      ENDIF
C
      CALL FLUSH(MUNIT)                                                 Mu
      CALL ABORT
C
      END

