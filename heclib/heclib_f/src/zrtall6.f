      SUBROUTINE zrtall6 (IFLTAB)
C
C
C     Retags all tags in a DSS file according to the tag
C     scheme set in the permanent section of the file.
C
C     Written by Bill Charley at HEC, 1989.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdsscm.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      INTEGER IFLTAB(*)
      CHARACTER CPATH*400
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zrtall6;  Unit:',I5)
      LTWCAT = .FALSE.
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zrtall6',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
      CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
C
C     Get the first pathname bin address
      IF (IFLTAB(KTABLE).EQ.1) THEN
      NBIN = IFLTAB(KBNBLK)
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
      NBIN = IFLTAB(KHASH)
      ELSE
C     We should never get here
      CALL zerror6 (IFLTAB, 100, 'zrtall6', 0, IFLTAB(KTABLE), ' ', 0,
     * ' ', 0)
      ENDIF
C
C     Get the address of the first bin
      IADD = IFLTAB(KAFBIN)
      JJBUFF = 1
      JJREC = -2
C
C     Now read all pathname bins from the file
 40   CONTINUE
C
C     Read the pathname bin
C     Release the previous record
      IF (JCREC(JJBUFF).EQ.JJREC) LSBUFF(JJBUFF) = .FALSE.
      NBNSIZ = IFLTAB(KBNSIZ)
      CALL zgtrec6 (IFLTAB, IPNBIN, NBNSIZ, IADD, .TRUE.)
C     This next lines keeps that area in memory while we read elsewhere
      JJBUFF = JBUFF
      JJREC = JCREC(JBUFF)
C
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
C     Record status good? (not deleted or renamed?)
      IF ((IPNBIN(JPNBIN).EQ.1).OR.(IPNBIN(JPNBIN).EQ.11)) THEN
C
C     Get pathname and pathname length
      NPATH = IPNBIN(JPNBIN+KBNPAT)
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CPATH, NPMWRD)
C
      CALL zgetag6 (IFLTAB, CPATH(1:NPATH), CTAG)
C
      CALL CHRHOL (CTAG, 1, NTAGC, IPNBIN(JPNBIN+NPPWRD+KBTAG), 1)
      I = IFLTAB(KBNSIZ)
      CALL zptrec6 (IFLTAB, IPNBIN, I, IADD, .FALSE.)
C
C     Get Information Block
      NADD = IPNBIN(JPNBIN+NPPWRD+KBAINF)
      CALL zgtrec6 (IFLTAB, INFO, NINFO+NPPWRD, NADD, .FALSE.)
C
C     Double Check that this is the correct pathname
      IF (NPATH.NE.INFO(KINPAT)) GO TO 900
C
C     Set record information block status flag
      CALL CHRHOL (CTAG, 1, NTAGC, INFO(NPPWRD+KITAG), 1)
      CALL zptrec6 (IFLTAB, INFO, NINFO+NPPWRD, NADD, .FALSE.)
C
C
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 200
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
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
C     No more bins or pathnames left.  All done
 300  CONTINUE
      IADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, IADD, .TRUE.)
      CALL zmultu6 ( IFLTAB, .FALSE., .TRUE.)
      IF (MLEVEL.GE.11) WRITE (MUNIT,320)
 320  FORMAT (T6,'-----DSS---Debug:  Exit  zrtall6')
      RETURN
C
C
 900  CONTINUE
      NP = INFO(KINPAT)
      CALL zerror6(IFLTAB, 11,'zrtall6',NP, IADD, CPATH, NPATH, ' ',NP)
C
      END
      SUBROUTINE zrtall (IFLTAB)
      INTEGER IFLTAB(*)
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call zrtall6 (IFLTAB)
      endif
      return
      end

