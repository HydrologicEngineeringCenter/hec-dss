      SUBROUTINE znwrit6 (IFLTAB, CPATH, NPATH, NIHEAD, NCHEAD, NUHEAD,
     * NDATA)
C
C
C     Main routine for writing a new record to a DSS file
C     Not User callable (use zwritex6)
C
C     Written by Bill Charley, HEC, June 1989
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
      INTEGER IFLTAB(*)
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      CHARACTER CTPATH*400, CPATH*(*)
C
      LOGICAL LONGPA
C
C
      IF (MLEVEL.GE.12) WRITE (MUNIT,20) IFLTAB(KUNIT), CPATH(1:NPATH),
     * NIHEAD, NCHEAD, NUHEAD, NDATA
 20   FORMAT (/,T10,'-----DSS---Debug: Enter znwrit6,  Unit:',I5,/,
     * /,T12,'Pathname: ',A,/,T12,'NIHEAD:',I5,', NCHEAD:',I5,
     * ', NUHEAD:',I5,', NDATA:',I5)
C
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      CTPATH = CPATH
      !CALL FortCopy(CTPATH, CPATH)
C
      IF (NPATH.GT.80) THEN
         LONGPA = .TRUE.
      ELSE
         LONGPA = .FALSE.
      ENDIF
C
C
C     Clear Last Pathname not found in zcheck6
      IFLTAB(KLPATL) = -1
C     Update effiency information variables
      IF (IPLOOP.EQ.0) THEN
      IFLTAB(KHUSED) = IFLTAB(KHUSED) + 1
      IF (IFLTAB(KTABLE).EQ.2) IFLTAB(KBINS) = IFLTAB(KBINS) + 1
      ENDIF
      IF (IPLOOP.GT.IFLTAB(KMAXPH)) THEN
      IFLTAB(KMAXPH) = IPLOOP
      IFLTAB(KMAXHC) = IHASH
      ENDIF
C
C     Get maximum part lengths for file
      IF (CPATH(1:1).EQ.'/') THEN
      CALL zupath (CPATH(1:NPATH), IBPART, IEPART, ILPART, J)
      IF (J.EQ.0) THEN
      DO 40 I=1,6
         IF (ILPART(I).GT.32) LONGPA = .TRUE.
         CALL GETHOL (IFLTAB(KMXPRT), I, IMXPRT)
         IF (ILPART(I).GT.IMXPRT) THEN
            IMXPRT = ILPART(I)
            CALL PUTHOL (IFLTAB(KMXPRT), I, IMXPRT)
         ENDIF
 40   CONTINUE
      ENDIF
      ENDIF
C
C
C     Update the current date and time
      CALL WHEN (CDATE, CTIME)
C
C
C     Write new pointers
C
C     If this pathname's hash code was not in the hash table
C     create a new bin, and save that bin's address in the table
      IF (.NOT.LINTAB) THEN
C
C     Write a new pathname bin
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-znwrit6:  Write new path bin'
      CALL znwbin6 (IFLTAB)
C
C     Write address of new pathname bin to hash location in table
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-znwrit6:  Save bin add in index'
      NADD = NPERM + IHASH
      CALL zptrec6 (IFLTAB, IPBADD, 1, NADD, .TRUE.)
C
      ELSE
C
C     A hash code already there (or file structure 2).  Make
C     sure that there is space in the bin for this pathname.
      I = JPNBIN + NPPWRD + NLBIN + 2
      IF (I.GT.IFLTAB(KBNSIZ)) THEN
C     No more space in this bin - create a new bin
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-znwrit6:  Add another path bin'
      IPNBIN(JPNBIN) = -1
      CALL znwbin6 (IFLTAB)
      ENDIF
      ENDIF
C
C     Save information about this record in the pathname bin
C     If this is a long pathname, set the status to 11, so older
C     versions of DSS will not look at it.
      IF (LONGPA) THEN
         IPNBIN(JPNBIN+KBSTAT) = 11
      ELSE
         IPNBIN(JPNBIN+KBSTAT) = 1
      ENDIF
      IPNBIN(JPNBIN+KBNPAT) = NPATH
      CALL CH2HOL (CTPATH, IPNBIN(JPNBIN+KBPATH), NPMWRD)
C     Point to new data area, which will start at end of file
      IPNBIN(JPNBIN+NPPWRD+KBAINF) = IFLTAB(KFSIZE)
      IPNBIN(JPNBIN+NPPWRD+KBNHEA) = NUHEAD
      IPNBIN(JPNBIN+NPPWRD+KBNDAT) = NDATA
      IF (NLDATA.GE.0) THEN
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NLDATA
      ELSE
      IPNBIN(JPNBIN+NPPWRD+KBLNDA) = NDATA
      ENDIF
      IPNBIN(JPNBIN+NPPWRD+KBHASH) = IHASH
      IPNBIN(JPNBIN+NPPWRD+KBPINTL) = INTL_PSEUDO
C
      IPNBIN(JPNBIN+NPPWRD+KBTAG+1) = 0
      IF (IRENAM.EQ.0) THEN
      IFLTAB(KNRECS) = IFLTAB(KNRECS) + 1
      IPNBIN(JPNBIN+NPPWRD+KBTYPE) = ITYPE
C     Generate a tag
      IF (ICHAR(CTAG(1:1)).LT.32) CTAG = ' '
      IF (CTAG(1:1).EQ.' ') CALL zgetag6 (IFLTAB, CPATH(1:NPATH), CTAG)
C     Be sure tag has no non-printable characters in it!
      DO 60 I=1, NTAGC
      IF (ICHAR(CTAG(I:I)).LT.32) CTAG(I:I) = ' '
 60   CONTINUE
      CALL CHRHOL (CTAG, 1, NTAGC, IPNBIN(JPNBIN+NPPWRD+KBTAG), 1)
      ELSE
      IPNBIN(JPNBIN+NPPWRD+KBTYPE) = IRNTYP
      CALL CHRHOL (CRNTAG, 1, NTAGC, IPNBIN(JPNBIN+NPPWRD+KBTAG), 1)
      ENDIF
C
C     Now store this bin with the new information
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-znwrit6:  Store path bin'
      I = INT(IFLTAB(KBNSIZ))
      CALL zptrec6 (IFLTAB, IPNBIN, I, IPBADD, .TRUE.)
C
C     Now create the information block
C
C     Update the file size to include this new block
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NINFO + NPPWRD
C     Store the pathname
      INFO(KIFLAG) = NPFLAG
C     If this is a long pathname, set the status to 11, so older
C     versions of DSS will not look at it.
      IF (LONGPA) THEN
         INFO(KISTAT) = 11
      ELSE
         INFO(KISTAT) = 1
      ENDIF
      INFO(KINPAT) = NPATH
      CALL CH2HOL (CTPATH, INFO(KIPATH), NPMWRD)
C
      IF (IRENAM.EQ.0) THEN
C     Store the header array location and length
      INFO(NPPWRD+KIAIHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINIHE) = NIHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NIHEAD
      INFO(NPPWRD+KIACHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINCHE) = NCHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NCHEAD
      INFO(NPPWRD+KIAUHE) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINUHE) = NUHEAD
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NUHEAD
C     Store the data array location and length
      INFO(NPPWRD+KIADAT) = IFLTAB(KFSIZE)
      INFO(NPPWRD+KINDAT) = NDATA
      IF (NLDATA.GE.0) THEN
      INFO(NPPWRD+KILNDA) = NLDATA
      ELSE
      INFO(NPPWRD+KILNDA) = NDATA
      ENDIF
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + NDATA
C     Store the data type, version number, date, time, and tag
      INFO(NPPWRD+KITYPE) = ITYPE
C     Compression flag (set to 0 for no compression)
      INFO(NPPWRD+KICOMP) = ICOMP
C     Alternative flag, usually used for data quality
      INFO(NPPWRD+KIQUAL) = IQUAL
      INFO(NPPWRD+KIVER) = IBVER
      CALL CHRHOL (CPROG, 1, NPROGC, INFO(NPPWRD+KIPROG), 1)
      CALL CHRHOL (CDATE, 1, NDATEC, INFO(NPPWRD+KIDATE), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, INFO(NPPWRD+KITIME), 1)
      INFO(NPPWRD+KITAG+NTAG-1) = 0
      CALL CHRHOL (CTAG, 1, NTAGC, INFO(NPPWRD+KITAG), 1)
      DO 80 I=1,NPASS
      INFO(NPPWRD+KIPASS+I-1) = 0
 80   CONTINUE
      INFO(NPPWRD+KIPREC) = IPREC
      INFO(NPPWRD+KIUNUS) = 0
C
      ENDIF
C
C     Store the information block
      ISIZE = NPPWRD + NINFO
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-znwrit6:  Store info block'
      CALL zptrec6 (IFLTAB, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .TRUE.)
C
C
      NADD = 1
      IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-znwrit6:  Update root'
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
C
 800  CONTINUE
C     Reset flags
      NLDATA = -1
      ITYPE = 0
      ICOMP = 0
      IQUAL = 0
      CTAG = ' '
      IRENAM = 0
      IF (MLEVEL.GE.12) WRITE (MUNIT,820)
 820  FORMAT (T8,'-----DSS---Debug: EXIT znwrit6')
      RETURN
C
      END

