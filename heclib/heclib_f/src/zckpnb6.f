      SUBROUTINE zckpnb6 (IFLTAB, IERROR)
C
C
C     Check a DSS file's Pathname Bin structure for
C     possible errors.
C
C     Written by Bill Charley at HEC, 1992.
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
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
C
C
      CHARACTER CPATH*400, CPATH2*400
      CHARACTER CABORT*3
      INTEGER IFLTAB(*)
      PARAMETER (KERR=20)
      LOGICAL LERR(20)
      INTEGER NPSTATS(15)
C
      integer iversion
C
C
      call zGetVersion(ifltab, iversion)
      if (iversion.eq.7) then
         call zcheckhashtable(ifltab, IERROR)
         return
      endif
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T8,'-----DSS---Debug:  Enter zckpnb6;  Unit:',I5)
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 30)
 30   FORMAT (/,' ERROR:  DSS File is not version 6, or file is not',
     * ' open.',/,' Only a version 6 DSS file can be checked.',/)
      GO TO 880
      ENDIF
C
C     The follow is if another thread interrupts us to stop the process
      INTERRUPT = 0
C
      DO 35 I=1,KERR
      LERR(I) = .FALSE.
 35   CONTINUE
C
C     Lock the file so we don't get erroneous results during
C     the check
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
      CALL zinqir6 (IFLTAB, 'ABORT', CABORT, I)
      CALL zset6 ('ABORT', 'OFF', I)
C
C     Step 1;  Search down all hash codes for all records
      MHASH = IFLTAB(KHASH)
      DO 38 I=1,15
         NPSTATS(I) = 0
 38   CONTINUE
      NERROR = 0
      TOTAL_NUMB = IFLTAB(KNRECS)
      CURRENT_NUMB = 0
C
      DO 200 IHASH=1,MHASH
C
C     If a Hash table is used, read the address for this hash code
      IF (IFLTAB(KTABLE).EQ.1) THEN
      IADD = NPERM + IHASH
      CALL zgtrec6 (IFLTAB, IPBADD, 1, IADD, .FALSE.)
      IF (IERRMS.NE.0) THEN
      IERRMS = 0
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
      IF (MLEVEL.GE.10) WRITE (MUNIT,40) IPBADD
 40   FORMAT (T12,'Table address:',I8)
C     Does a pathname bin exist for this hash code?  (Exit if no)
      IF (IPBADD.EQ.0) GO TO 200
C
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
C
C     If no Hash table is used (type 2), read the bin directly
      IPBADD = ((IHASH - 1) * IFLTAB(KBNSIZ)) + IFLTAB(KAFBIN)
C
      ELSE
C     Should not get here
      WRITE (MUNIT,*)' IFLTAB CORRUPT IN zckpnb6, KTABLE DOES NOT MATCH'
      CALL zabort6 (IFLTAB, 100, 'zckpnb6', 0, IFLTAB(KTABLE), ' ')
      IERROR = NERROR
      RETURN
      ENDIF
C
C
C     Read the pathname bin
 60   CONTINUE
      ISIZE = IFLTAB(KBNSIZ)
      CALL zgtrec6 (IFLTAB, IPNBIN, ISIZE, IPBADD, .FALSE.)
      IF (IERRMS.NE.0) THEN
      IERRMS = 0
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
      JPNBIN = 1
C
C     Loop through the pathname bin, looking at pathnames
 80   CONTINUE
C     If another thead set the interrupt flag, return
      IF (INTERRUPT.NE.0) GO TO 880
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) THEN
      GO TO 200
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more paths in this bin (bin full) - go to next path bin
      N = IFLTAB(KBNSIZ) - 1
      IPBADD = IPNBIN(N)
      IF (IPBADD.GT.IFLTAB(KFSIZE)) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 90) JHASH, IPBADD, IFLTAB(KFSIZE)
 90   FORMAT (/' *** Error:  Next Pathname Bin address is greater than',
     * ' the file size.',/,'  Error for hash code',I6,
     * ',  Bin Address:',I15,',  File Size:',I15)
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
      GO TO 60
      ENDIF
C
C     Yes
      NBWPAT = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
C
      IPSTAT = IPNBIN(JPNBIN)
      IF ((IPSTAT.GE.1).AND.(IPSTAT.LE.15)) THEN
         NPSTATS(IPSTAT) = NPSTATS(IPSTAT) + 1
      ELSE
      NERROR = NERROR + 1
      IF (MLEVEL.GE.1) WRITE (MUNIT, 100) IPBADD, IPNBIN(JPNBIN),JHASH
 100  FORMAT (/,' *** Invalid Pathname Bin at address',I9,/,
     * '     Invalid record status detected: ',I12,'  Hash:',I5)
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      GO TO 200
      ENDIF
C
C     Check the length of the pathname
      NPATH = IPNBIN(JPNBIN+KBNPAT)
      IF ((NPATH.LT.4).OR.(NPATH.GT.392)) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 110) IPBADD, IPNBIN(JPNBIN+KBNPAT)
 110  FORMAT (/,' *** Invalid Pathname Bin at address',I9,/,
     * '     Invalid pathname length detected:  Length',I12)
      NERROR = NERROR + 1
      IF (MLEVEL.GE.7) CALL ZDEBUG (MUNIT, IPNBIN, IPBADD, ISIZE)
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      GO TO 200
      ENDIF
C
C     Check the pathname
      NPPWRD = (NPATH - 1) / NCPW + 1
      NPMWRD = (NPATH - 1) / NCMW + 1
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CPATH, NPMWRD)
      CURRENT_NUMB = CURRENT_NUMB + 1
C
C     Check data values and addresses
      JADD   = IPNBIN(JPNBIN+KBAINF+NBWPAT)
      NHEAD  = IPNBIN(JPNBIN+KBNHEA+NBWPAT)
      NDATA  = IPNBIN(JPNBIN+KBNDAT+NBWPAT)
      NLDATA = IPNBIN(JPNBIN+KBLNDA+NBWPAT)
      JTYPE  = IPNBIN(JPNBIN+KBTYPE+NBWPAT)
      JHASH  = IPNBIN(JPNBIN+KBHASH+NBWPAT)
C
C     Get the information block
      JSIZE = NINFO+NPPWRD
      CALL zgtrec6 (IFLTAB, INFO, JSIZE, JADD, .FALSE.)
      IF (IERRMS.NE.0) THEN
      IERRMS = 0
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
C
C     Check the values in the information block
      IF (INFO(KIFLAG).NE.NPFLAG) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 120) JADD, INFO(KIFLAG),
     * NPFLAG, CPATH(1:NPATH)
 120  FORMAT (/,' *** Invalid information block at address',I12,/,
     * '  The information flag is invalid:',I12,' should be',I12,/,
     * '  Pathname: ',A)
      NERROR = NERROR + 1
      IF (MLEVEL.GE.7) CALL ZDEBUG (MUNIT, INFO, JADD, JSIZE)
      GO TO 190
      ENDIF
C
      CALL HOL2CH (INFO(KIPATH), CPATH2, NPMWRD)
C
      IF (IPNBIN(JPNBIN).NE.INFO(KISTAT)) LERR(2 ) = .TRUE.
      IF (NPATH.NE.INFO(KINPAT)) LERR(3 ) = .TRUE.
      IF (CPATH(1:NPATH).NE.(CPATH2(1:NPATH))) LERR(4 ) = .TRUE.
      IF (INFO(KINDAT+NPPWRD).NE.NDATA)  LERR(5 ) = .TRUE.
*     IF (INFO(KILNDA+NPPWRD).NE.NLDATA) LERR(6 ) = .TRUE.
      IF (INFO(KINUHE+NPPWRD).NE.NHEAD)  LERR(7 ) = .TRUE.
      IF (INFO(KITYPE+NPPWRD).NE.JTYPE)  LERR(8 ) = .TRUE.
      IF (INFO(KICOMP+NPPWRD).LT.-5)     LERR(9 ) = .TRUE.
      IF (INFO(KICOMP+NPPWRD).GT.20)     LERR(10) = .TRUE.
      IF (INFO(KIQUAL+NPPWRD).LT.-5)     LERR(11) = .TRUE.
      IF (INFO(KIQUAL+NPPWRD).GT.20)     LERR(12) = .TRUE.
      IF (INFO(KINIHE+NPPWRD).LT.0)      LERR(13) = .TRUE.
      IF (INFO(KINCHE+NPPWRD).LT.0)      LERR(14) = .TRUE.
C
      IF ((INFO(KINDAT+NPPWRD).GT.0).AND.((INFO(KIADAT+NPPWRD).LT.0)
     * .OR.(INFO(KIADAT+NPPWRD).GT.IFLTAB(KFSIZE)))) LERR(15) = .TRUE.
      IF ((INFO(KINIHE+NPPWRD).GT.0).AND.((INFO(KIAIHE+NPPWRD).LT.0)
     * .OR.(INFO(KIAIHE+NPPWRD).GT.IFLTAB(KFSIZE)))) LERR(16) = .TRUE.
      IF ((INFO(KINCHE+NPPWRD).GT.0).AND.((INFO(KIACHE+NPPWRD).LT.0)
     * .OR.(INFO(KIACHE+NPPWRD).GT.IFLTAB(KFSIZE)))) LERR(17) = .TRUE.
      IF ((INFO(KINUHE+NPPWRD).GT.0).AND.((INFO(KIAUHE+NPPWRD).LT.0)
     * .OR.(INFO(KIAUHE+NPPWRD).GT.IFLTAB(KFSIZE)))) LERR(18) = .TRUE.
C
C
      IERRS = 0
      DO 125, I=1,KERR
      IF (LERR(I)) IERRS = IERRS + 1
 125  CONTINUE
C
C     We made it
      IF (IERRS.EQ.0) THEN
      IF (MLEVEL.GE.7) WRITE (MUNIT, 130) CPATH(1:NPATH)
 130  FORMAT (' Record Checks: ',A)
      IF (MLEVEL.GE.8) WRITE (MUNIT, 140) JHASH, IPBADD, JPNBIN,
     * JADD, JTYPE, NDATA
 140  FORMAT ('  Hash',I5,',   Bin Add:',I9,',   JPNBIN:',I5,/,
     * '  Info Add:',I9,',    Type:',I4,',    Number of data:',I6)
C
      ELSE
C
      IF (MLEVEL.GE.2) WRITE (MUNIT, 150) IERRS, CPATH(1:NPATH)
 150  FORMAT (/,' *** Invalid information block,',
     * '  Number of errors:',I4,/,'  Pathname: ',A,/,
     * '  List of errors for this record:')
      IF (MLEVEL.GE.3) WRITE (MUNIT, 140) JHASH, IPBADD, JPNBIN,
     * JADD, JTYPE, NDATA
      NERROR = NERROR + 1
      IF (MLEVEL.GE.3) THEN
      IF (LERR(2)) WRITE (MUNIT, 151) IPNBIN(JPNBIN), INFO(KISTAT)
 151  FORMAT ('  Bin status does not match info block status.',/,
     * '  Bin status:',I8,',  Info status:',I8)
      IF (LERR(3)) WRITE (MUNIT, 152) NPATH, INFO(KINPAT)
 152  FORMAT ('  Length of pathname does not match info block.',/,
     * '  Length in Bin:',I8,',  Length in info block:',I8)
      IF (LERR(4)) WRITE (MUNIT, 153) CPATH(1:NPATH), CPATH2(1:NPATH)
 153  FORMAT ('  Pathname in Bin does not match info block.',/,
     * '  Bin Path:  ',A,/,'  Info Path: ',A)
      IF (LERR(5)) WRITE (MUNIT, 154) INFO(KINDAT+NPPWRD), NDATA
 154  FORMAT ('  Number of data in bin does not match info block.',/,
     * '  Number in Bin:',I8,',  Number in info block:',I8)
*     IF (LERR(6)) WRITE (MUNIT, 155) NLDATA, INFO(KILNDA+NPPWRD)
 155  FORMAT ('  Logical Number of data in bin does not match info',
     * ' block.',/,'  Number in Bin:',I8,',  Number in info block:',I8)
      IF (LERR(7)) WRITE (MUNIT, 156) INFO(KINUHE+NPPWRD), NHEAD
 156  FORMAT ('  Length of user header does not match info block.',/,
     * '  Length in Bin:',I8,',  Length in info block:',I8)
      IF (LERR(8)) WRITE (MUNIT, 157) JTYPE, INFO(KITYPE+NPPWRD)
 157  FORMAT ('  Data type in Bin does not match info block.',/,
     * '  Type in Bin:',I8,',  Type in info block:',I8)
      IF (LERR(9)) WRITE (MUNIT, 158) INFO(KICOMP+NPPWRD)
 158  FORMAT ('  Invalid data compression flag in info block.',/,
     * '  Data compression flag:',I8)
      IF (LERR(10)) WRITE (MUNIT, 158) INFO(KICOMP+NPPWRD)
      IF (LERR(11)) WRITE (MUNIT, 160) INFO(KIQUAL+NPPWRD)
 160  FORMAT ('  Invalid data quality flag in info block.',/,
     * '  Data quality flag:',I8)
      IF (LERR(12)) WRITE (MUNIT, 160) INFO(KIQUAL+NPPWRD)
      IF (LERR(13)) WRITE (MUNIT, 162) INFO(KINIHE+NPPWRD)
 162  FORMAT ('  Invalid internal header size in info block.',/,
     * '  Internal header size:',I12)
      IF (LERR(14)) WRITE (MUNIT, 163) INFO(KINCHE+NPPWRD)
 163  FORMAT ('  Invalid compression header size in info block.',/,
     * '  Compression header size:',I12)
      IF (LERR(15)) WRITE (MUNIT, 164) INFO(KIADAT+NPPWRD)
 164  FORMAT ('  Invalid data address in info block; Address:',I12)
      IF (LERR(16)) WRITE (MUNIT, 165) INFO(KIAIHE+NPPWRD)
 165  FORMAT ('  Invalid internal header address in info block;',
     * ' Address:',I12)
      IF (LERR(17)) WRITE (MUNIT, 166) INFO(KIACHE+NPPWRD)
 166  FORMAT ('  Invalid compression header address in info block;',
     * ' Address:',I12)
      IF (LERR(18)) WRITE (MUNIT, 167) INFO(KIAUHE+NPPWRD)
 167  FORMAT ('  Invalid user header address in info block;',
     * ' Address:',I12)
      ENDIF
      IF (MLEVEL.GE.7) CALL ZDEBUG (MUNIT, INFO, JADD, JSIZE)
      DO 180 I=1,KERR
      LERR(I) = .FALSE.
 180  CONTINUE
      ENDIF
C
C
C     Check next path in block (unless no more)
 190  CONTINUE
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      JPNBIN = JPNBIN + NBWPAT + NLBIN
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 200
      GO TO 80
C
C
 200  CONTINUE
C
C
 800  CONTINUE
C
      NTOTAL = NPSTATS(1) + NPSTATS(11)
      IF (NTOTAL.NE.IFLTAB(KNRECS)) THEN
      NERROR = NERROR + 1
      IF (MLEVEL.GE.1) WRITE (MUNIT, 810) IFLTAB(KNRECS), NTOTAL
 810  FORMAT (/,' *** zckpnb6:  Inconsistency in the number of records',
     * ' in the file.',/,' Number Recorded: ',I6,',  Number Found:',I6)
      ENDIF
C
      IF ((MLEVEL.GE.1).AND.(NERROR.GT.0)) WRITE (MUNIT, *)' '
      IF (MLEVEL.GE.1) WRITE (MUNIT, 820) NERROR
 820  FORMAT (' Pathname Bin Check Complete:',I6,
     * ' Records with errors found.')
      IF (MLEVEL.GE.3) WRITE (MUNIT, 825) NTOTAL
 825  FORMAT (' Total Number of Records:  ',I7)
      IF (MLEVEL.GE.3) WRITE (MUNIT, 830) (NPSTATS(I),I=1,4)
 830  FORMAT (' Number of Regular Records:',I7,/,
     * ' Number of Deleted Records: ',I6,/,
     * ' Number of Renamed Records: ',I6,/,
     * ' Number of Replaced Records:',I6)
C
      IF (MLEVEL.GE.3) WRITE (MUNIT, 840) (NPSTATS(I),I=11,14)
 840  FORMAT (' Number of Long Pathname Records:        ',I7,/,
     * ' Number of Deleted Long Pathname Records: ',I6,/,
     * ' Number of Renamed Long Pathname Records: ',I6,/,
     * ' Number of Replaced Long Pathname Records:',I6)
C
C
 880  CONTINUE
      IERROR = NERROR
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
      CALL zset6 ('ABORT', CABORT, I)
      IF (MLEVEL.GE.12) WRITE (MUNIT,890) IFLTAB(KUNIT)
 890  FORMAT (T8,'-----DSS---Debug:  Exit zckpnb6;   Unit:',I5)
C
      RETURN
C
      END
      SUBROUTINE zckpnb(IFLTAB, IERROR)
      INTEGER IFLTAB(*)
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
        call zckpnb6 (IFLTAB, IERROR)
      else
        call zcheckPathnameBins(IFLTAB, IERROR)
      endif
      return
      end

