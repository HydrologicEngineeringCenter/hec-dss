      SUBROUTINE zckpat6 (IFLTAB, IERROR)
C
C
C     Check all pathnames in a DSS file.
C     (Similar to checking all records in a catalog.)
C
C     Written by Bill Charley at HEC, 1993.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
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
      INTEGER IFLTAB(*), IPBIN(128)
      CHARACTER CPATH*400
C
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
      INTEGER NERROR, MAXERROR
C
      integer iversion
C
C
      call zGetVersion(ifltab, iversion)
      if (iversion.eq.7) then
         call zcheckpathnames(ifltab, IERROR)
         return
      endif
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zckpat6;  Unit:',I5)
C
C     Lock the file, so we will not get incorrect results
C     if someone else is writting to the file.
      CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
      CALL zset6 ('ABORT', 'OFF', I)
      NRECS = IFLTAB(KNRECS)
      JNPATH = 0
      NERROR = 0
C     The follow is if another thread interrupts us to stop the process
      INTERRUPT = 0
      TOTAL_NUMB = IFLTAB(KNRECS)
      CURRENT_NUMB = 0
C
C     Get the first pathname bin address
      IF (IFLTAB(KTABLE).EQ.1) THEN
      NBIN = IFLTAB(KBNBLK)
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
      NBIN = IFLTAB(KHASH)
      ELSE
C     We should never get here
      GO TO 900
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
      CALL zgtrec6 (IFLTAB, IPBIN, NBNSIZ, IADD, .TRUE.)
      IF (IERRMS.NE.0) THEN
      IERRMS = 0
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
C     This next lines keeps that area in memory while we read elsewhere
      JJBUFF = JBUFF
      JJREC = JCREC(JBUFF)
      JPBIN = 1
C
C     Loop through bin, looking for pathnames
 100  CONTINUE
       IF (INTERRUPT.NE.0) GO TO 800
C     Any more pathnames left?
      IF (IPBIN(JPBIN).EQ.0) GO TO 200
C     Yes - Compute the number of integer words in the pathname
      NBPWPA = ((IPBIN(JPBIN+KBNPAT)-1) / NCPW) + 1
      NBMWPA = ((IPBIN(JPBIN+KBNPAT)-1) / NCMW) + 1
C
C     Record status good? (not deleted or renamed?)
      IF ((IPBIN(JPBIN).EQ.1).OR.(IPBIN(JPBIN).EQ.11)) THEN
C
      JNPATH = JNPATH + 1
C
C     Get pathname and pathname length
      NPATH = IPBIN(JPBIN+KBNPAT)
      CALL HOL2CH (IPBIN(JPBIN+KBPATH), CPATH, NBMWPA)
C
C     Now check the record and read the info block.
      CALL zrdinf6 (IFLTAB, CPATH(1:NPATH), NHEAD, NDATA, ISTAT)
C
      CURRENT_NUMB = CURRENT_NUMB + 1
      IF (ISTAT.EQ.0) THEN
      IF (MLEVEL.GE.7) WRITE (MUNIT, 120) CPATH(1:NPATH)
 120  FORMAT (' Record Checks: ',A)
      ELSE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 140) IADD, CPATH(1:NPATH)
 140  FORMAT (' *** zckpat6, Unable to find record from address:',
     * I12,/,' Pathname: ',A)
      NERROR = NERROR + 1
      IF ((NERROR.GE.MAXERROR).AND.(MAXERROR.GT.0)) GO TO 800
      ENDIF
C
C
C     Have we reached the number of records in the file?
C     IF (JNPATH.GE.NRECS) GO TO 800
C
C
      ELSE IF (IPBIN(JPBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 200
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
 180  CONTINUE
      JPBIN = JPBIN + NBPWPA + NLBIN
C     Is that pointer too large?
      IF (JPBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 200
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
C     Any more bins in the DSS file? (Exit to 800 if no more).
      IF (IPBIN(I).EQ.0) GO TO 800
      IADD = IPBIN(I)
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
 800  CONTINUE
      IF (NERROR.EQ.0) THEN
C        Check that the number checked is the same as in header
         if (IFLTAB(KNRECS).ne.CURRENT_NUMB) then
           NERROR = NERROR + 1
           IF (MLEVEL.GE.1)WRITE(MUNIT, 810)CURRENT_NUMB,IFLTAB(KNRECS)
 810  FORMAT (' *** zckpat6, Inconsistent Number of Records',/,
     *  ' Number found:',I8,'  Number Recorded:',I8)
         endif
      ENDIF
      IERROR = NERROR
      CALL zmultu6 (IFLTAB, .FALSE., .TRUE.)
      CALL zset6 ('ABORT', 'ON', I)
C
      IF (MLEVEL.GE.1) WRITE (MUNIT, 820) NERROR
 820  FORMAT (/,' Pathname Check Complete,',I4,' Errors found.')
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,840)
 840  FORMAT (T6,'-----DSS---Debug:  Exit  zckpat6')
      RETURN
C
C
 900  CONTINUE
      CALL zerror6 (IFLTAB, 100, 'zckpat6', 0, IFLTAB(KTABLE), ' ', 0,
     * ' ', 0)
C
      END
      SUBROUTINE zckpat(IFLTAB, IERROR)
      INTEGER IFLTAB(*)
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
        call zckpat6 (IFLTAB, IERROR)
      else
        call zcheckPathnames(IFLTAB, IERROR)
      endif
      return
      end

