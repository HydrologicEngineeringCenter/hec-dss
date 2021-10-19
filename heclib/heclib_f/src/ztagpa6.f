      SUBROUTINE ztagpa6 (IFLTAB, JUNIT, CTAGS, NTAGS, CPATH, NPATH,
     * NFOUND)
C
C
C     Given an array of tags (may be one tag), ztagpa6 finds
C     the corresponding pathname(s).
C     This routine quickly searches the DSS file for the pathnames
C     (not the catalog file), and is the prefered means of
C     obtaining pathnames from tags.
C     If JUNIT is greater than zero, the pathnames are written to
C     unit JUNIT (in a form readable by zrdcat6) instead of being
C     placed in array CPATH.
C     NFOUND returns the number of pathnames found.
C     NTAGS can be no more than 50.
C     NPATH is an integer array that MUST be dimensioned to NTAGS
C     (regardless if JUNIT is used).
C
C     Written by Bill Charley at HEC, January 1990.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsscm.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdssmz.h'
C
C
      INTEGER IFLTAB(*), NPATH(*)
      INTEGER JUNIT, NTAGS, NFOUND
      CHARACTER CTAGS(*)*(*), CPATH(*)*(*)

      CHARACTER CT*10, CT2*10, CP*400

      PARAMETER (MXTAGS=50)
      INTEGER IHCODE(MXTAGS)
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT), NTAGS, CTAGS(1)
 20   FORMAT (T6,'-----DSS---Debug:  Enter ztagpa6;  Unit:',I5,/,
     * T11,'NTAGS:',I4,'   First Tag: -',A,'-')
C
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'ztagpa6',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
C     Clear arrays
      JFOUND = 0
      NFOUND = 0
C
C     If we are writing pathname out to a file, clear that file
      IF (JUNIT.GT.0) THEN
      REWIND JUNIT
      WRITE (JUNIT, *, ERR=920, IOSTAT=JERR) ' '
      REWIND JUNIT
      ENDIF
C
      IF (NTAGS.GT.MXTAGS) GO TO 910
      DO 40 I=1,NTAGS
      NPATH(I) = 0
      IHCODE(I) = 0
 40   CONTINUE
      JTAG = NTAG * NWPW
C
C     Read the permanent section of the file
      CALL zrdprm6 (IFLTAB, .FALSE.)
C
C     If the tag-hash code block exists, get it.
      IF (IFLTAB(KTAGBK).GT.0) THEN
C
      CALL zgtrec6 (IFLTAB, ILBUFF, NTAGBK, IFLTAB(KTAGBK), .FALSE.)
      IBPOS = 3
C
 100  CONTINUE
      CALL HOL2CH (ILBUFF(IBPOS), CT, JTAG)
C
      DO 120 I=1,NTAGS
      IF (IHCODE(I).EQ.0) THEN
      CT2 = CTAGS(I)
      IF (CT2(1:NTAGC).EQ.CT(1:NTAGC)) THEN
      IHCODE(I) = ILBUFF(IBPOS+2)
      JFOUND = JFOUND + 1
      IF (JFOUND.EQ.NTAGS) GO TO 200
      ENDIF
      ENDIF
 120  CONTINUE
C
      IBPOS = IBPOS + 3
C
      IF (IBPOS.GE.NTAGBK-2) THEN
      IADD = ILBUFF(NTAGBK)
      IF (IADD.LE.0) THEN
C     No more records left!
      GO TO 200
      ELSE
C     Read next block
      CALL zgtrec6 (IFLTAB, ILBUFF, NTAGBK, IADD, .FALSE.)
      IBPOS = 3
      GO TO 100
      ENDIF
      ENDIF
C
      GO TO 100
C
C
C     ************************************************
C
 200  CONTINUE
C     We have retrieved hash codes for the tags passed in.
C     Get their pathnames
C
      IF (JFOUND.GT.0) THEN
C
      DO 280 I=1,NTAGS
C
      IF ((IHCODE(I).GT.0).AND.(NPATH(I).EQ.0)) THEN
      IF (IFLTAB(KTABLE).EQ.1) THEN
      IADD = NPERM + IHCODE(I)
      CALL zgtrec6 (IFLTAB, IPBADD, 1, IADD, .FALSE.)
C     Does a pathname bin exist for this hash code?  (Exit if no)
      IF (IPBADD.EQ.0) GO TO 280
C
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
C     If no Hash table is used (type 2), read the bin directly
      IPBADD = ((IHCODE(I) - 1) * IFLTAB(KBNSIZ)) + IFLTAB(KAFBIN)
C
      ELSE
      GO TO 900
      ENDIF
C
 220  CONTINUE
C     Read the pathname bin
      ISIZE = IFLTAB(KBNSIZ)
      CALL zgtrec6 (IFLTAB, IPNBIN, ISIZE, IPBADD, .FALSE.)
      JPNBIN = 1
C
C     Loop through the pathname bin, looking for this pathname
 240  CONTINUE
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) GO TO 280
      NBWPAT = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
      IF ((IPNBIN(JPNBIN).EQ.1).OR.(IPNBIN(JPNBIN).EQ.11)) THEN
      CALL HOL2CH (IPNBIN(JPNBIN+NBWPAT+KBTAG), CT, JTAG)
C
      CT2 = CTAGS(I)
      IF (CT(1:NTAGC).EQ.CT2(1:NTAGC)) THEN
C     Found it!
      NPATH(I) = IPNBIN(JPNBIN+KBNPAT)
      N = ((NPATH(I)-1) / NCMW) + 1
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CP, N)
      NFOUND = NFOUND + 1
      IF (JUNIT.GT.0) THEN
      WRITE (JUNIT,260) NFOUND, CT(1:NTAGC), CP(1:NPATH(I))
 260  FORMAT (I6,2X,A,4X,A)
      ELSE
      CPATH(I) = CP(1:NPATH(I))
      ENDIF
      IF (NFOUND.EQ.NTAGS) GO TO 800
      GO TO 280
      ENDIF
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more paths in this bin (bin full) - go to next path bin
      N = IFLTAB(KBNSIZ) - 1
      IPBADD = IPNBIN(N)
      GO TO 220
      ENDIF
C     Check next path in block (unless no more)
      JPNBIN = JPNBIN + NBWPAT + NLBIN
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 280
      GO TO 240
C
      ENDIF
C
 280  CONTINUE
C
      ENDIF
C
      ENDIF
C
C
C     ************************************************
C
C     Some tags not in the tag-hash code table.
C     Do a bute force search, looking for the other tags
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
 300  CONTINUE
C
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
 320  CONTINUE
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) GO TO 360
C     Yes - Compute the number of integer words in the pathname
      NBPWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
      NBMWPA = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCMW) + 1
C
C     Record status good? (not deleted or renamed?)
      IF ((IPNBIN(JPNBIN).EQ.1).OR.(IPNBIN(JPNBIN).EQ.11)) THEN
C
C     Yes.
      CALL HOL2CH (IPNBIN(JPNBIN+KBTAG+NBPWPA), CT, JTAG)
C
      DO 340 I=1,NTAGS
      IF (NPATH(I).EQ.0) THEN
      CT2 = CTAGS(I)
      IF (CT(1:NTAGC).EQ.CT2(1:NTAGC)) THEN
C     Found it!
      NPATH(I) = IPNBIN(JPNBIN+KBNPAT)
      N = ((NPATH(I)-1) / NCMW) + 1
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CP, N)
      NFOUND = NFOUND + 1
      IF (JUNIT.GT.0) THEN
      WRITE (JUNIT,260) NFOUND, CT(1:NTAGC), CP(1:NPATH(I))
      ELSE
      CPATH(I) = CP(1:NPATH(I))
      ENDIF
      IF (NFOUND.EQ.NTAGS) GO TO 800
      ENDIF
      ENDIF
 340  CONTINUE
C
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more space in this block, read the next one
      GO TO 360
      ENDIF
C
C     Update the bin pointer (to next possible pathname location
C     within this block).
      JPNBIN = JPNBIN + NBPWPA + NLBIN
C     Is that pointer too large?
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 360
C     Go back and look for next pathnme within this bin
      GO TO 320
C
C
 360  CONTINUE
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
      GO TO 300
C
C
C     No more bins or pathnames left.  All done
 800  CONTINUE
      LSBUFF(1) = .FALSE.
      IF (MLEVEL.GE.11) WRITE (MUNIT,820) NFOUND
 820  FORMAT (T6,'-----DSS---Debug:  Exit  ztagpa6;  Numb Found:',I4)
      RETURN
C
C
 900  CONTINUE
      CALL zabort6 (IFLTAB, 100, 'ztagpa6', 0, IFLTAB(KTABLE), ' ')
      RETURN
C
 910  CONTINUE
      WRITE (MUNIT,911) NTAGS, MXTAGS
 911  FORMAT (/' -----DSS***  ZGETPA;  Error:  Too Many Tags Provided',
     * ' ***',/,' Number Given:',I5,',   Maximum:',I5,/)
      GO TO 800
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,921) JUNIT, JERR
 921  FORMAT (/' -----DSS--- ztagpa6:  Error during Write, Unit:',I5,
     * /,' Error:',I5,/)
      GO TO 800
C
      END
      SUBROUTINE ztagpa (IFLTAB, JUNIT, CTAGS, NTAGS, CPATH, NPATH,
     * NFOUND)
      INTEGER IFLTAB(*), NPATH(*)
      INTEGER JUNIT, NTAGS, NFOUND
      CHARACTER CTAGS(*)*(*), CPATH(*)*(*)
      call zGetVersion(IFLTAB, iversion)
      IF (iversion.EQ.6) THEN
      call ztagpa6 (IFLTAB, JUNIT, CTAGS, NTAGS, CPATH, NPATH,
     * NFOUND)
      endif
      return
      end

