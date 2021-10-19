      SUBROUTINE zcklnk6 (IFLTAB, IERROR)
C
C
C     Check a DSS file's Links for possible errors.
C     (Can find all the records_.
C
C     Written by Bill Charley at HEC, 1993.
C
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      CHARACTER CPATH*400
      INTEGER IFLTAB(*), IARRAY(NBSIZE)
      LOGICAL LFOUND
      INTEGER NPSTATS(15), NMOVED
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
      INTEGER NERROR, MAXERROR
      integer iversion
C
C
      call zGetVersion(ifltab, iversion)
      if (iversion.eq.7) then
         call zchecklinks(ifltab, IERROR)
         return
      endif
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T8,'-----DSS---Debug:  Enter zcklnk6;  Unit:',I5)
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 30)
 30   FORMAT (/,' ERROR:  DSS File is not version 6, or file is not',
     * ' open.',/,' Only a version 6 DSS file can be checked.',/)
      GO TO 880
      ENDIF
C
C     Lock the file so we don't get erroneous results during
C     the check
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
      CALL zset6 ('ABORT', 'OFF', I)
C     The following is if another thread interrupts us to stop the process
      INTERRUPT = 0
      CURRENT_NUMB = 0
C
C     Search for all information blocks in the file.
      NERROR = 0
      NMOVED = 0
      DO 38 I=1,15
         NPSTATS(I) = 0
 38   CONTINUE
C
C     Get the number of physical records in file (not data recs)
      IADD = IFLTAB(KFSIZE) - 1
      CALL zgetrw6 (IADD, NRECS, IW)
      TOTAL_NUMB = NRECS
C
      DO 200 IREC=2,NRECS
      CURRENT_NUMB = IREC
      IF (INTERRUPT.NE.0) GO TO 880
C
C     Read physical record IREC from file
      CALL zgetad6 (IADD, IREC, 1)
      CALL zgtrec6 (IFLTAB, IARRAY, NBSIZE, IADD, .FALSE.)
      IF (IERRMS.NE.0) THEN
      IERRMS = 0
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
C
C     Search through this record, looking for pathname flags
C
      DO 199 IWRD=1,NBSIZE
C
      IF (IARRAY(IWRD).EQ.NPFLAG) THEN
C
C     Found a flag - Get the first three words of the information
C     block to see if this indeed is the start of a data record
      CALL zgetad6 (IADD, IREC, IWRD)
      CALL zgtrec6 ( IFLTAB, INFO, 3, IADD, .FALSE.)
      IF (IERRMS.NE.0) THEN
      IERRMS = 0
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
C
      IPSTAT = INFO(KISTAT)
      IF ((IPSTAT.GE.1).AND.(IPSTAT.LE.15)) THEN
         NPSTATS(IPSTAT) = NPSTATS(IPSTAT) + 1
      ELSE IF (IPSTAT.EQ.-1) THEN
         NMOVED = NMOVED + 1
      ENDIF
C
C     Check for a valid status flag (1)
      IF ((IPSTAT.EQ.1).OR.(IPSTAT.EQ.11)) THEN
C     Check for a valid pathname length
      NPATH = INFO(KINPAT)
      IF ((NPATH.GT.4).AND.(NPATH.LE.392)) THEN
C
C     Passed, therefore a valid record - Get the full information block
      NPPWRD = (NPATH - 1)/NCPW + 1
      NPMWRD = (NPATH - 1)/NCMW + 1
      CALL zgtrec6 ( IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
      IF (IERRMS.NE.0) THEN
      IERRMS = 0
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
C
C     Get the pathname
      CPATH = ' '
      CALL HOL2CH (INFO(KIPATH), CPATH, NPMWRD)
C
      CALL zcheck6 (IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
C
      IF (LFOUND) THEN
      JADD = IPNBIN (JPNBIN+NPPWRD+KBAINF)
C
      IF (JADD.NE.IADD) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 100) CPATH(1:NPATH), JADD, IADD
 100  FORMAT (/' *** zcklnk6: Lost address link.  Bin address does not',
     * ' match info address',/,' Pathname: ',A,/,
     * ' Bin address:',I12,',  Info address:',I12)
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ELSE
      IF (MLEVEL.GE.7) WRITE (MUNIT, 120) CPATH(1:NPATH)
 120  FORMAT (' Record Checks: ',A)
      ENDIF
C
      ELSE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 140) CPATH(1:NPATH)
 140  FORMAT(/' *** zcklnk6:  Lost address link.  Unable to find record',
     * /,' Pathname: ',A)
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
C
      ENDIF
      ENDIF
      ENDIF
C
C
C
 199  CONTINUE
 200  CONTINUE
C
C
 800  CONTINUE
C
      NTOTAL = NPSTATS(1) + NPSTATS(11)
      IF (NTOTAL.NE.IFLTAB(KNRECS)) THEN
      NERROR = NERROR + 1
      IF (MLEVEL.GE.1) WRITE (MUNIT, 810) IFLTAB(KNRECS), NTOTAL
 810  FORMAT (' *** zcklnk6:  Inconsistency in the number of records',
     * ' in the file.',/,' Number Recorded: ',I6,',  Number Found:',I6)
      ENDIF
C
      IF ((MLEVEL.GE.1).AND.(NERROR.GT.0)) WRITE (MUNIT, *)' '
      IF (MLEVEL.GE.1) WRITE (MUNIT, 820) NERROR
 820  FORMAT (' Pathname Link Check Complete,',I4,' Errors found.')
      IF (MLEVEL.GE.3) WRITE (MUNIT, 830) (NPSTATS(I),I=1,4), NMOVED
 830  FORMAT (' Number of Records:        ',I7,/,
     * ' Number of Deleted Records: ',I6,/,
     * ' Number of Renamed Records: ',I6,/,
     * ' Number of Replaced Records:',I6)
C
      IF (MLEVEL.GE.3) WRITE (MUNIT, 840) (NPSTATS(I),I=11,14)
 840  FORMAT (' Number of Long Pathname Records:        ',I7,/,
     * ' Number of Deleted Long Pathname Records: ',I7,/,
     * ' Number of Renamed Long Pathname Records: ',I7,/,
     * ' Number of Replaced Long Pathname Records:',I7,/,
     * ' Number of Moved Records:   ',I7)
C
 880  CONTINUE
      IERROR = NERROR
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
      CALL zset6 ('ABORT', 'ON', I)
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,890) IFLTAB(KUNIT)
 890  FORMAT (T8,'-----DSS---Debug:  Exit zcklnk6;   Unit:',I5)
C
      RETURN
C
      END
      SUBROUTINE zcklnk(IFLTAB, IERROR)
      INTEGER IFLTAB(*)
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
        call zcklnk6 (IFLTAB, IERROR)
      else
        call zchecklinks(IFLTAB, IERROR)
      endif
      return
      end

