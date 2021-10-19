      SUBROUTINE zndata6 (IFLTAB, IFPOS, JULS, ISECS, CPATH, NPATH,
     *                   JULD, ISECD, IDTYPE, ISTAT)
C
C
C     New Data
C     Return pathnames for new data written to the file since
C     a given date/time.  Pathnames are returned one at a time,
C     for subsequent calls until the end of the file is reached.
C     To start list, set IFPOS to 0.  At the end of the list
C     ISTAT will be set to 1 (with no pathname returned), otherwise
C     ISTAT will be 0 for a normal return with pathname, or <0 for
C     an (somewhat serious) error return.
C
C      Input:
C        IFLTAB - From zopen6
C        IFPOS - File position, set to zero at start
C        ISTIME - Start time, in minutes since Jan 1, 1900,
C                 to check for data later than
C
C     Output:
C        CPATH - Pathname returned
C        NPATH - Length of pathname
C        IPTIME - Time of last write
C        ISTAT -  <0:  Error
C                  0:  Pathname retrieved okay
C                  1:  Reached end of file.  No pathname returned
C
C     Written by Bill Charley at HEC, 2004.
C
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsscz.h'
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
      INTEGER IFLTAB(*), IFPOS, NPATH, ISTAT
      CHARACTER CPATH*(*)

C
      INTEGER IPBIN(128)
      LOGICAL LMATCH
      CHARACTER CCDATE*10, CCTIME*10
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zndata6;  Unit:',I5)
C
C
C     Have we reached the end of the file
      IF (IFPOS.GE.IFLTAB(KFSIZE)) GO TO 850
C
C
C
      IF (IFPOS.LE.0) THEN
C        Get the first pathname bin address, and
C        the address of the first bin
         CALL zrdprm6(IFLTAB, .FALSE.)
         NBIN = IFLTAB(KBNBLK)
         IADD = IFLTAB(KAFBIN)
         JPBIN = 1
      ELSE
         IADD   = IFPOS
         JPBIN  = IFLTAB(KEY3+1)
         NBIN   = IFLTAB(KEY3+2)
         ISELCA = IFLTAB(KEY3+3)
C
         IF (JPBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 200
      ENDIF
C
C
C     Now read pathname bin from the file
 40   CONTINUE
C     Do a quick check of the address
      IF (IADD.LE.0) THEN
         ISTAT = -2
         GO TO 900
      ENDIF
      IF (IADD.GT.IFLTAB(KFSIZE)) THEN
C         Get an update of the file size
          CALL zrdprm6(IFLTAB, .FALSE.)
C         If the address is just a little off, then someone else
C         is writing to the file while we are reading it.
C         If it is way off, then it is a file error
          ISTAT = -3
          IF ((IADD+20000).GT.IFLTAB(KFSIZE)) GO TO 900
		IF (IADD.GT.IFLTAB(KFSIZE)) GO TO 850
      ENDIF
      CALL zgtrec6 (IFLTAB, IPBIN, IFLTAB(KBNSIZ), IADD, .FALSE.)
C
C
C     Loop through bin, looking for pathnames
 100  CONTINUE
C     Any more pathnames left?
      IF (IPBIN(JPBIN).LE.0) GO TO 200
C     Yes - Compute the number of integer words in the pathname
      NBPWPA = ((IPBIN(JPBIN+KBNPAT)-1) / NCPW) + 1
      NBMWPA = ((IPBIN(JPBIN+KBNPAT)-1) / NCMW) + 1
C
C     Record status good? (not deleted or renamed?)
      IF ((IPBIN(JPBIN).EQ.1).OR.(IPBIN(JPBIN).EQ.11)) THEN
C        Valid record.  Get pathname and pathname length
         NPATH = IPBIN(JPBIN+KBNPAT)
         CALL HOL2CH (IPBIN(JPBIN+KBPATH), CKPATH, NBMWPA)
C        Read its information block to compare last written times
         NADD = IPBIN(JPBIN+NBPWPA+KBAINF)
         CALL zgtrec6 (IFLTAB, INFO, NINFO+NBPWPA, NADD, .FALSE.)
C
C        Extract information from this array
         IF (INFO(KIFLAG).NE.NPFLAG) GO TO 900
         IDTYPE = INFO(NBPWPA+KITYPE)
         CCDATE = ' '
         CCTIME = ' '
         CALL HOLCHR (INFO(NBPWPA+KIDATE), 1, NDATEC, CCDATE, 1)
         CALL HOLCHR (INFO(NBPWPA+KITIME), 1, NTIMEC, CCTIME, 1)
         CALL DATJUL(CCDATE(1:NDATEC), JULD, IERR)
         ISECD = IHMS2S(CCTIME(1:NTIMEC))
	   LMATCH = .FALSE.
         IF ((JULD.GE.0).AND.(ISECD.GE.0)) THEN
            IF (JULD.GT.JULS) THEN
               LMATCH = .TRUE.
            ELSE IF (JULD.EQ.JULS) THEN
               IF (ISECD.GE.ISECS) LMATCH = .TRUE.
            ENDIF
         ENDIF
         IF (LMATCH) THEN
            CPATH = CKPATH
C	      Update the bin pointer
            JPBIN = JPBIN + NBPWPA + NLBIN
            GO TO 800
         ENDIF
      ENDIF
C
C     Do an error check on this block
      IF ((IPBIN(JPBIN).LT.-1000).OR.(IPBIN(JPBIN).GT.1000)) THEN
         ISTAT = -4
         GO TO 900
      ENDIF
C
C     Unmatched, deleted or renamed record.
C     Update the bin pointer (to next possible pathname
C     location  within this block).
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
C        No - Get pointer to next bin block (section).
C        Get location of pointers in bin
         I = IFLTAB(KBNSIZ)
C        Any more bins in the DSS file? (Exit to 850 if no more).
         IF (IPBIN(I).EQ.0) GO TO 850
         IADD = IPBIN(I)
         NBIN = IFLTAB(KBNBLK)
      ELSE
C        More bins available within this block - get next one.
         IADD = IADD + IFLTAB(KBNSIZ)
      ENDIF
C
C     Go back up and read next bin
      JPBIN = 1
      GO TO 40
C
C
 800  CONTINUE
      ISTAT = 0
C     Save pointer information
      IFPOS = IADD
      IFLTAB(KEY3+1) = JPBIN
      IFLTAB(KEY3+2) = NBIN
      IFLTAB(KEY3+3) = ISELCA
C
 820  CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT,840)
 840  FORMAT (T6,'-----DSS---Debug:  Exit  zckpat6')
      RETURN
C
C
 850  CONTINUE
C     End of file
      ISTAT = 1
      IFPOS = IFLTAB(KFSIZE)
      CPATH = ' '
      NPATH = 0
      IFLTAB(KEY3+1) = 0
      IFLTAB(KEY3+2) = 0
      IFLTAB(KEY3+3) = 0
      GO TO 820
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE(MUNIT,920)
 920  FORMAT(' -----DSS---Error:  File error detected in zndata6')
C      CALL zerror6 (IFLTAB, 100, 'zndata6', 0, IFLTAB(KTABLE), ' ', 0,
C     * ' ', 0)
      IF (ISTAT.EQ.-2) THEN
      IF (MLEVEL.GE.1) WRITE(MUNIT,930) IADD
 930  FORMAT(' zndata6 Error: File address out of range:',I10)
      ELSE IF (ISTAT.EQ.-3) THEN
	IF (MLEVEL.GE.1) WRITE(MUNIT,930) IADD
      ELSE IF (ISTAT.EQ.-4) THEN
      IF (MLEVEL.GE.1) WRITE(MUNIT,940) IPBIN(JPBIN)
 940  FORMAT(' zndata6 Error: Pathname status out of range:',I10)
      ENDIF
      IF (ISTAT.GE.0) ISTAT = -1
      GO TO 820
C
      END

