      SUBROUTINE zudava6 (IFLTAB, CPATH, ISTATUS)
C
C
C     Z UnDelete AVAilable
C     Find all deleted records in a DSS file that can be undeleted
C     (prior to being removed by a squeeze).
C
C     ISTATUS is both an input and output parameter.
C     For the first call set to 0.
C     On subsequent calls, do not change.
C     When the end of the file is reached, ISTATUS will be
C     returnted with -1 (with CPATH blanked)
C
C     Written by Bill Charley at HEC, 1999.
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
      INCLUDE 'zdssmz.h'
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*(*)
C
      CHARACTER CTPATH*392
C
C     The next line is very important for this implementation!
      SAVE
C
C
      IF (MLEVEL.GE.12) WRITE ( MUNIT, 20)
 20   FORMAT (T8,'-----DSS---Debug: Enter zudava6')
C
C
C
      IF (ISTATUS.LE.0) THEN
C        Lock file, and read the permanent section
         CALL zrdprm6 (IFLTAB, .TRUE.)
C
C        Get the first pathname bin address
         IF (IFLTAB(KTABLE).EQ.1) THEN
            NBIN = IFLTAB(KBNBLK)
         ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
            NBIN = IFLTAB(KHASH)
         ELSE
C           We should never get here
            CALL zerror6(IFLTAB, 100,'zudall6', 0, IFLTAB(KTABLE), ' ',
     *                   0, ' ', 0)
         ENDIF
C
C        Get the address of the first bin
         IADD = IFLTAB(KAFBIN)
         ISTATUS = 1
      ELSE
         IF (IADD.GE.IFLTAB(KFSIZE)) THEN
            ISTATUS = -1
            CPATH = ' '
            RETURN
         ENDIF
         GO TO 140
      ENDIF
C
C     Now read all pathname bins from the file
 40   CONTINUE
C
C     Read the pathname bin
      I = IFLTAB(KBNSIZ)
      CALL zgtrec6 (IFLTAB, IPNBIN, I, IADD, .FALSE.)
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
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CTPATH, NPMWRD)
C     Should we show a pseudo regular interval pathname?
      IF (LPSEUDO.AND.(IPNBIN(JPNBIN+NPMWRD+KBPINTL).GT.0)) THEN
            CALL ZPseudoRTS6(CTPATH(1:NPATH), CPATH,
     *                 IPNBIN(JPNBIN+NPMWRD+KBPINTL), 2, ISTAT)
         IF (ISTAT.NE.0) THEN
            CPATH = CTPATH(1:NPATH)
         ENDIF
      ELSE
         CPATH = CTPATH(1:NPATH)
      ENDIF
      RETURN
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
C     No more bins or pathnames left.
 300  CONTINUE
      ISTATUS = -1
      CPATH = ' '
      RETURN
      END

