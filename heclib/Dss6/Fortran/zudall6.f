      SUBROUTINE zudall6 (IFLTAB, JUNIT)
C
C
C     Undelete all records in a DSS file that have previously
C     been deleted (prior to being removed by a squeeze).
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      INTEGER IFLTAB(*)
      LOGICAL LFOUND
      CHARACTER CPATH*400
C
C
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20)
 20   FORMAT (T8,'-----DSS---Debug: Enter zudall6')
C
C
C
C     Lock file, and read the permanent section
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
      LFOUND = .FALSE.
C
C     Get the first pathname bin address
      IF (IFLTAB(KTABLE).EQ.1) THEN
      NBIN = IFLTAB(KBNBLK)
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
      NBIN = IFLTAB(KHASH)
      ELSE
C     We should never get here
      CALL zerror6 (IFLTAB, 100, 'zudall6', 0, IFLTAB(KTABLE), ' ', 0,
     * ' ', 0)
      ENDIF
C
C     Get the address of the first bin
      IADD = IFLTAB(KAFBIN)
C
C     Now read all pathname bins from the file
 40   CONTINUE
C
C     Read the pathname bin
      I = IFLTAB(KBNSIZ)
      CALL zgtrec6 (IFLTAB, IPNBIN, I, IADD, .FALSE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 800
      JPNBIN = 1
C
C     Loop through bin, looking for pathnames
 100  CONTINUE
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) GO TO 200
C     Yes - Compute the number of integer words in the pathname
      NPPWRD = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
      NPMWRD = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCMW) + 1
C
C     Check the record status.
      IF ((IPNBIN(JPNBIN).EQ.1).OR.(IPNBIN(JPNBIN).EQ.11)) THEN
C     Status OK.
C
      ELSE IF ((IPNBIN(JPNBIN).EQ.2).OR.(IPNBIN(JPNBIN).EQ.12)) THEN
C
C     A deleted record.  Undelete it.
C
      NPATH = IPNBIN(JPNBIN+KBNPAT)
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CPATH, NPMWRD)
      NHEAD = IPNBIN(JPNBIN+KBNHEA+NPPWRD)
      NDATA = IPNBIN(JPNBIN+KBNDAT+NPPWRD)
      LFOUND = .TRUE.
C
      IF (JUNIT.GT.0) THEN
      WRITE (JUNIT, 105) CPATH(1:NPATH)
 105  FORMAT (' ---zundel6;  Available:  ',A)
      GO TO 140
      ENDIF
C
C     Undelete bin status
      IF (IPNBIN(JPNBIN).EQ.12) THEN
         IPNBIN(JPNBIN) = 11
      ELSE
         IPNBIN(JPNBIN) = 1
      ENDIF
C     Save it
      I = IFLTAB(KBNSIZ)
      CALL zptrec6 (IFLTAB, IPNBIN, I, IADD, .FALSE.)
C
C     Change status in Information Block
      JADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, KISTAT, JADD, .FALSE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 800
      IF (INFO(KIFLAG).NE.-9753) THEN
      WRITE (MUNIT,110) CPATH(1:NPATH)
 110  FORMAT (/,' -----DSS---zundel6  ERROR:  Bad Address Detected',/,
     * ' Pathname: ',A,/)
      GO TO 140
      ELSE
      INFO(KISTAT) = IPNBIN(JPNBIN)
      CALL zptrec6 (IFLTAB, INFO, KISTAT, JADD, .FALSE.)
      ENDIF
C
C     Write informative message
C
      IF (MLEVEL.GE.3) WRITE (MUNIT,120) IFLTAB(KUNIT), CPATH(1:NPATH)
 120  FORMAT (1X,'-----DSS---zundel6 Unit',I5,':  ',A)
C
      IFLTAB(KNRECS) = IFLTAB(KNRECS) + 1
      IFLTAB(KDEAD) = IFLTAB(KDEAD)  -(NINFO + NPPWRD + NHEAD + NDATA)
C
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 200
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
 140  CONTINUE
      JPNBIN = JPNBIN + NPPWRD + NLBIN
C     Is that pointer too large?
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 200
C     Go back and look for next pathnme within this bin
      GO TO 100
C
C
 200  CONTINUE
C     At this point, there are no more pathnames in the current bin
C     (or it is full and extends into another block).
C     Read the next pathname bin
      NBIN = NBIN - 1
C     Any more bins in this block?
      IF (NBIN.LE.0) THEN
C     No - Get pointer to next bin block (section).
C     Get location of pointers in bin
      I = IFLTAB(KBNSIZ)
C     Any more bins in the DSS file? (Exit to 300 if no more).
      IF (IPNBIN(I).EQ.0) GO TO 300
      IADD = IPNBIN(I)
      NBIN = IFLTAB(KBNBLK)
      ELSE
C     More bins available within this block - get next one.
      IADD = IADD + IFLTAB(KBNSIZ)
      ENDIF
C
C     Go back up and read next bin
      GO TO 40
C
C
C     No more bins or pathnames left.  Store new dead area info
 300  CONTINUE
      IF (JUNIT.LE.0) THEN
      IADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .FALSE.)
      ENDIF
      IF (.NOT.LFOUND) WRITE (MUNIT, 310)
 310  FORMAT (' ---zundel6;  No records available to undelete.')
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
C
 800  CONTINUE
      IFLTAB(KLPATL) = -1
      IF (MLEVEL.GE.12) WRITE ( MUNIT,820)
 820  FORMAT (T8,'-----DSS---Debug:  Exit zudall6')
      CALL FLUSH(MUNIT)                                                 Mu
C
      RETURN
C
C
      END

