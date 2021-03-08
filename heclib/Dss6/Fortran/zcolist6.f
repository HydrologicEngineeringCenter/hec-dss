      SUBROUTINE zcolist6 (IFLTAB, IFPOS, CPATH, NPATH, ISTAT)
C
C
C     Get the set of collection pathnames in a DSS file
C     Return one at a time.
C     To start list, set IFPOS to 0.  CPATH needs to contain one
C     of the pathnames from the collection.  At the end of the list
C     ISTAT will be set to 1 (with no pathname returned), otherwise
C     ISTAT will be 0 for a normal return with pathname, or <0 for
C     an (somewhat serious) error return.
C
C
C
C     Written by Bill Charley at HEC.
C
C
      INCLUDE 'zdsskz.h'
C
*      INCLUDE 'zdsslz.h'
C
*      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssnz.h'
C
*      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
C
      INTEGER IFLTAB(*), IFPOS, NPATH, ISTAT
      CHARACTER CPATH*(*)
C
      INTEGER IPBIN(128), JHASH, MHASH, IPBADD, JPBIN
      INTEGER IADD, IPSTAT, NBMWPA, NBWPAT, N
      LOGICAL LCOLL
      CHARACTER CKPATH*393, CCPATH*393, CLPATH*393
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT), CPATH(1:NPATH)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zcolist6;  Unit:',I5,/,
     * T6,'Seed path = ',A)
C
C
C     Have we reached the end of the file
      IF (IFPOS.GE.IFLTAB(KFSIZE)) GO TO 850
C
      CALL zcolpath6(CPATH, NPATH, CCPATH, LCOLL)
      IF (.NOT.LCOLL) GO TO 850
C
      IF (IFPOS.LE.0) THEN
C        If this is the first entrance, get the hash code
C        Get the first pathname bin address, and
C        the address of the first bin
         CALL zrdprm6(IFLTAB, .FALSE.)
         MHASH = IFLTAB(KHASH)
         CALL zhash6(CCPATH, NPATH, MHASH, JHASH)
         JPBIN = 1
C        If a Hash table is used, read the address for this hash code
         IF (IFLTAB(KTABLE).EQ.1) THEN
            IADD = NPERM + JHASH
            CALL zgtrec6 (IFLTAB, IPBADD, 1, IADD, .FALSE.)
            IF (MLEVEL.GE.10) WRITE (MUNIT,45) IPBADD
 45         FORMAT (T12,'Table address:',I8)
C           Does a pathname bin exist for this hash code?  (Exit if no)
            IF (IPBADD.EQ.0) GO TO 850
         ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
C           If no Hash table is used (type 2), read the bin directly
            IPBADD = ((JHASH - 1) * IFLTAB(KBNSIZ)) + IFLTAB(KAFBIN)
         ELSE
C           Should not get here
            WRITE(MUNIT,*)' IFLTAB CORRUPT IN zcolist6, KTABLE ',
     *                    IFLTAB(KTABLE)
            CALL zabort6(IFLTAB, 100,'zcolist6', 0, IFLTAB(KTABLE), ' ')
            RETURN
        ENDIF
      ELSE
C        Set the address and pointes to the end of the last path read
         IPBADD = IFPOS
         JPBIN  = IFLTAB(KEY3+1)
c         IF (JPBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 200
      ENDIF
C
C
C     Read pathname bin from the file
 40   CONTINUE
C     Do a quick check of the address
      IF (IPBADD.LE.0) THEN
         ISTAT = -2
         GO TO 900
      ENDIF
      IF (IPBADD.GT.IFLTAB(KFSIZE)) THEN
C         Get an update of the file size
          CALL zrdprm6(IFLTAB, .FALSE.)
C         If the address is just a little off, then someone else
C         is writing to the file while we are reading it.
C         If it is way off, then it is a file error
          ISTAT = -3
          IF ((IPBADD+20000).GT.IFLTAB(KFSIZE)) GO TO 900
		IF (IPBADD.GT.IFLTAB(KFSIZE)) GO TO 850
      ENDIF
      CALL zgtrec6 (IFLTAB, IPBIN, IFLTAB(KBNSIZ), IPBADD, .FALSE.)
C     Did we get an error on the last read?
      IF (IFLTAB(KSTAT).NE.0) THEN
         GO TO 900
      ENDIF
C
C     Loop through the pathname bin, looking for this pathname
 80   CONTINUE
C     Any more pathnames left?
      IF (IPBIN(JPBIN).EQ.0) GO TO 850
C
C     Yes - See if it is the correct one.
C     Compute the number of integer words in the pathname
      NBMWPA = ((IPBIN(JPBIN+KBNPAT)-1) / NCMW) + 1
      NBWPAT = ((IPBIN(JPBIN+KBNPAT)-1) / NCPW) + 1
      IPSTAT = IPBIN(JPBIN)
      IF ((IPSTAT.EQ.1).OR.(IPSTAT.EQ.11)) THEN
C
C        Check the length of the pathname
         IF (NPATH.EQ.IPBIN(JPBIN+KBNPAT)) THEN
C           Same length - check the pathnames
            CALL HOL2CH (IPBIN(JPBIN+KBPATH), CLPATH, NBMWPA)
            CALL zcolpath6(CLPATH, NPATH, CKPATH, LCOLL)
            IF (CCPATH(1:NPATH).EQ.CKPATH(1:NPATH)) THEN
C              Same Pathnames - record found.
               call strcpy(CPATH, CLPATH(1:NPATH))
C              Save this pathname's name and location
              IFLTAB(KPADD) = IPBADD
              IFLTAB(KPJBIN) = JPBIN
              JPBIN = JPBIN + NBWPAT + NLBIN
              IF (JPBIN.GT.(IFLTAB(KBNSIZ)-2)) THEN
                GO TO 850
              ENDIF
              IF (IPBIN(JPBIN).EQ.-1) THEN
                 N = IFLTAB(KBNSIZ) - 1
                 IPBADD = IPBIN(N)
                 JPBIN = 1
              ENDIF
              GO TO 800
           ENDIF
        ENDIF
        GO TO 100
C
      ELSE IF (IPBIN(JPBIN).EQ.-1) THEN
C        No more paths in this bin (bin full) - go to next path bin
         N = IFLTAB(KBNSIZ) - 1
         IPBADD = IPBIN(N)
         JPBIN = 1
         GO TO 40
      ENDIF
C
 100  CONTINUE
C     Check next path in block (unless no more)
      JPBIN = JPBIN + NBWPAT + NLBIN
      IF (JPBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 850
      GO TO 80
C
C
 800  CONTINUE
      ISTAT = 0
C     Save pointer information
      IFPOS = IPBADD
      IFLTAB(KEY3+1) = JPBIN
C
 820  CONTINUE
      IF (MLEVEL.GE.11) THEN
         WRITE (MUNIT,840) ISTAT
 840     FORMAT (T6,'-----DSS---Debug:  Exit  zcolist6, Status:', I3)
         IF (ISTAT.EQ.0) THEN
            WRITE (MUNIT, 841) IFPOS, JPBIN, CPATH(1:NPATH)
 841        FORMAT(' Path found at bin file address: ',I8,
     *             '  Bin index:',I4,/,' Pathname: ',A)
         ELSE IF (ISTAT.EQ.1) THEN
            WRITE (MUNIT, 842)
 842        FORMAT(' No (more) pathnames for collection')
         ELSE
            WRITE (MUNIT, 843)
 843        FORMAT( '     ---- Error -----')
         ENDIF
      ENDIF
      RETURN
C
C
  850  CONTINUE
C     End of bins
      ISTAT = 1
      IFPOS = IFLTAB(KFSIZE)
      CPATH = ' '
      NPATH = 0
      IFLTAB(KLPATL) = 0
      IFLTAB(KEY3+1) = 0
      GO TO 820
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE(MUNIT,920)
 920  FORMAT(' -----DSS---Error:  File error detected in zcolist6')
C      CALL zerror6 (IFLTAB, 100, 'zplist6', 0, IFLTAB(KTABLE), ' ', 0,
C     * ' ', 0)
      IF (ISTAT.EQ.-2) THEN
      IF (MLEVEL.GE.1) WRITE(MUNIT,930) IADD
 930  FORMAT(' zcolist6 Error: File address out of range:',I10)
      ELSE IF (ISTAT.EQ.-3) THEN
	IF (MLEVEL.GE.1) WRITE(MUNIT,930) IADD
      ELSE IF (ISTAT.EQ.-4) THEN
      IF (MLEVEL.GE.1) WRITE(MUNIT,940) IPBIN(JPBIN)
 940  FORMAT(' zcolist6 Error: Pathname status out of range:',I10)
      ENDIF
      IF (ISTAT.GE.0) ISTAT = -1
      GO TO 820
C
      END

