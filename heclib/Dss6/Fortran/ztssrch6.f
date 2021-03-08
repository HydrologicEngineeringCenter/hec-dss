      SUBROUTINE ztssrch6 (IFLTAB, CIPATH, IUNIT, COUTPATH, NFOUND)
      implicit none
C
C     Time Series Search.
C     Given a DSS file IFLTAB and a time series pathname,
C     search the database for data sets with the same pathname,
C     except for the "D" (Date) part.  Either the first pathname
C     found (same except for the D part) can be returned (FAST),
C     or a list of all pathnames that match can be written to
C     a file (complete but slow, depending on the file size).
C     This routine is typically used when you have parts of the pathname
C     but you do not know when the data occurs.
C
C     IFLTAB - (input) Array from the zopen6 call.
C     CIPATH - (input) Character string containing the pathname to
C               search.  The D part is ignored (usually ts blank).
C     IUNIT - (input)  If a complete list is to be found, this should
C               be the unit number of the file to write the list to.
C               If only the first pathname is to be returned, set this
C               to zero.
C     COUTPATH - (output) Character string containing the pathname
C               of the first matching path, if IUNIT is zero.
C               If IUNIT is positive, this will be blank.
C     NFOUND - (output)  The number of pathnames found.  -1 if an error
C               occured, zero if none.
C
C     Written by Bill Charley at HEC, 1998.
C
C
      INCLUDE 'zdsskz.h'
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
      CHARACTER CIPATH*(*), COUTPATH*(*)
      CHARACTER CPATH*392, CIP*392, COP*392, CINPATH*392
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      integer NFOUND,I,INTLPS,ISTAT,I4TH,I5TH,NP,NINPATH
      integer J5TH,NRECS,JNPATH,NBIN,IADD,JJBUFF,JJREC
      integer NBNSIZ,NBPWPA,NPATH,IUNIT,NBMWPA
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter ztssrch6;  Unit:',I5)
C
      NFOUND = 0
      COUTPATH = ' '
      CINPATH = CIPATH
C
C     Be sure we are not using a pseudo pathname
      I = INDEX(CIPATH, '/~')
      IF (I.GT.2) THEN
        CALL ZPseudoRTS6(CIPATH, CINPATH, INTLPS, 1, ISTAT)
      ENDIF
C
C     Get the location of the "D" part (the 4th and 5th slash)
      CALL zupath (CINPATH, IBPART, IEPART, ILPART, ISTAT)
      IF (ISTAT.NE.0) GO TO 940
      I4TH = IBPART(4) - 1
      I5TH = IEPART(4) + 1
      IF (ILPART(4).EQ.0) I5TH = I5TH - 1
C
C     Determine the length of the (real) pathname
      CALL CHRLNB (CINPATH, NP)
      NINPATH = (NP - I5TH) + I4TH + 10
C     This is the 5 slash for pathnames in the file
      J5TH = I4TH + 10
C
C
C     Read the permanent section of the file.
      CALL zrdprm6 (IFLTAB, .FALSE.)
      NRECS = IFLTAB(KNRECS)
      JNPATH = 0
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
C
C     Get the address of the first bin
      IADD = IFLTAB(KAFBIN)
      JJBUFF = 1
      JJREC = -2
C
C     Now read pathname bins from the file
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
      JPNBIN = 1
C
C     Loop through bin, looking for pathnames
 100  CONTINUE
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) GO TO 200
C     Yes - Compute the number of integer words in the pathname
      NBPWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
      NBMWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCMW) + 1
C
C     Record status good? (not deleted or renamed?)
      IF ((IPNBIN(JPNBIN).EQ.1).OR.(IPNBIN(JPNBIN).EQ.11)) THEN
C
      JNPATH = JNPATH + 1
C
C     Yes.
C
C     Get pathname and pathname length
      NPATH = IPNBIN(JPNBIN+KBNPAT)
C
      IF (NPATH.EQ.NINPATH) THEN
         CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CPATH, NBMWPA)
C        Now check for the match
         CIP = CINPATH
         COP = CPATH
         CALL UPCASE(CIP)
         CALL UPCASE(COP)
         IF ((CIP(1:I4TH).EQ.COP(1:I4TH)).AND.
     *       (CIP(I5TH:NP).EQ.COP(J5TH:NPATH))) THEN
C           Found one!
C           Return this one
            IF (IUNIT.LE.0) THEN
               COUTPATH = CPATH(1:NPATH)
               NFOUND = 1
               GO TO 800
            ELSE
               WRITE (IUNIT,'(A)') CPATH(1:NPATH)
               NFOUND = NFOUND + 1
            ENDIF
         ENDIF
      ENDIF
C
C     Have we reached the number of records in the file?
      IF (JNPATH.GE.NRECS) GO TO 800
C
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 200
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
 180  CONTINUE
      JPNBIN = JPNBIN + NBPWPA + NLBIN
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
C     Any more bins in the DSS file? (Exit to 800 if no more).
      IF (IPNBIN(I).EQ.0) GO TO 800
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
 800  CONTINUE
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug:  Exit  ztssrch6')
      RETURN
C
C
 900  CONTINUE
      CALL zerror6 (IFLTAB, 100, 'ztssrch6', 0, IFLTAB(KTABLE), ' ', 0,
     * ' ', 0)
C
 940  CONTINUE
      NFOUND = -2
      GO TO 800
C
      END

