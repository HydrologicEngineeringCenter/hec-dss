      SUBROUTINE zcheck6 ( IFLTAB, CPATH, NPATH, NHEAD, NDATA, LFOUND)
      implicit none
C
C     Basic subroutine to find location of pathname record in
C     DSS file.  Returns the length of the header and data
C     areas.  The location of the record is passed back to zwrite6
C     and zread6 via commons.
C
C     Written by Bill Charley at HEC, 1988.
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
C
      COMMON /WORDS/ IWORD(10)
C
      CHARACTER CPATH*(*)
      CHARACTER CLPATH*400
      INTEGER IFLTAB(*)
      integer NPATH,NDATA,NHEAD,I,ICH,ISTAT,IERR
      integer MHASH,NADD,IADD,ISIZE,NBWPAT,IPSTAT,N,IWORD
      LOGICAL LFOUND
      LOGICAL LCOLL
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,20) IFLTAB(KUNIT),
     *                  NPATH, CPATH(1:NPATH)
 20   FORMAT (T8,'-----DSS---Debug:  Enter zcheck6;  Unit:',I5,
     * ',  NPATH:',I4,/,T4,'Pathname: ',A)
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zcheck6',
     * 0, IFLTAB, ' ', 0, ' ',0)
C
C
C
      IF (LBWRIT) GO TO 900
C
C     Initialization
      LFOUND = .FALSE.
      NDATA = 0
      NHEAD = 0
      NPMWRD = (NPATH - 1)/NCMW + 1
      NPPWRD = (NPATH - 1)/NCPW + 1
C
C     Be sure that we have room to use this size pathname
      IF ((NPPWRD+NLBIN+2).GT.IFLTAB(KBNSIZ)) THEN
         IF (MLEVEL.GE.1) WRITE (MUNIT, 10) NPATH, CPATH(1:NPATH)
 10      FORMAT(/,' -----DSS---ERROR:  Length of pathname is too long',
     *           ' for this file.',/,' Length: ',I5,',  Pathname: ',A)
          IFLTAB(KSTAT) = 340
         GO TO 100
      ENDIF
C
C     IPLOOP counts how many paths have this hash code - for
C     information only (effiency of the file)
      IPLOOP = 0
C
C     Force all characters in the pathname to be upper case
      DO 25 I=1,NPATH
      ICH = ICHAR(CPATH(I:I))
      IF (ICH.LT.32) THEN
      CPATH(I:I) = '?'
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)
     * 'Illegal Pathname Character at ',I,ICH
      ELSE IF ((ICH.GT.96).AND.(ICH.LT.123)) THEN
      IF (ICH.LT.123) CPATH(I:I) = CHAR(ICH-32)
      IF (MLEVEL.GE.14) WRITE (MUNIT,*)
     * 'Lowercase Pathname Character at ',I,ICH
      ENDIF
 25   CONTINUE
C
C     Save the pathname
      CKPATH = CPATH(1:NPATH)
C
C     Look for pseudo regular time series (PRTS), an
C     irregular interval that behaves like a regular interval
      I =  INDEX(CKPATH(1:NPATH), '/~')
      IF (I.GT.5) THEN
      CLPATH = ' '
         CALL ZPseudoRTS6(CKPATH, CLPATH, I, 1, ISTAT)
         IF (ISTAT.EQ.0) THEN
            CKPATH = CLPATH
            CALL CHRLNB(CKPATH, NPATH)
         ENDIF
      ENDIF
C
C     Check to see if the last pathname accessed is the same one -
C     Are they the same length?
      IF (NPATH.EQ.IFLTAB(KLPATL)) THEN
C
C     Yes - Compare Pathnames
      CALL HOL2CH ( IFLTAB(KLPATH), CLPATH, NPMWRD)
      IF (CKPATH(1:NPATH).EQ.CLPATH(1:NPATH)) THEN
      LINTAB = .FALSE.
      IF (IFLTAB(KINTAB).EQ.1) LINTAB = .TRUE.
C     Was the Found flag set to Found
      IF (IFLTAB(KLPFOU).EQ.1) THEN
      IF (MLEVEL.GE.12) WRITE (MUNIT,30) 'Found'
 30   FORMAT (T10,'-*- Record Previously Checked: ',A,'  -*-')
      ELSE
      IF (MLEVEL.GE.12) WRITE (MUNIT,30) 'NOT Found'
C     On a write, force the file to be read so the correct
C     hash code and table flag is set.
      IF (LWRITE) GO TO 40
      GO TO 100
      ENDIF
C
C     If another record from a different unit has been accessed in
C     the mean time, reload this pathname bin
      IPBADD = IFLTAB(KPADD)
      JPNBIN  = IFLTAB(KPJBIN)
      NSIZE = IFLTAB(KBNSIZ)
      IF ((IPBADD.GT.IFLTAB(KFSIZE)).AND.(.NOT.LWRITE)) GO TO 40
      CALL zgtrec6 (IFLTAB, IPNBIN, NSIZE, IPBADD, .FALSE.)
C
C     Now double check to ensure the file has not changed
      IF ((IPNBIN(JPNBIN).EQ.0).OR.((IPNBIN(JPNBIN).EQ.2.)
     * .AND.(.NOT.LUNDEL))) GO TO 40
      IF (NPATH.NE.IPNBIN(JPNBIN+KBNPAT)) GO TO 40
C
      LFOUND = .TRUE.
C     Report back the header and data length
      NHEAD = IFLTAB(KPNHEA)
      NDATA = IFLTAB(KPNDAT)
      GO TO 100
      ENDIF
      ENDIF
C
C
C
C     No - Check the normal way
C
 40   CONTINUE
C
C     If on a PC, on a read, do we need to check for an advisory lock?
C     (On a write, zmultu6 will check)
      IF (.NOT.LWRITE) THEN
         CALL zckmul6 (IFLTAB)
         IF (IFLTAB(KMULT).EQ.2) CALL zrdprm6 (IFLTAB, .TRUE.)
      ENDIF
C
C
C     On the PC, if we are in user access mode 2, then someone
C     else is writing to the file, and we need to be sure our
C     buffers are updated from disk (read only)
      IF ((.NOT.LWRITE).AND.(IFLTAB(KMULT).EQ.2)) THEN                  Md
         CALL flushf (IFLTAB(KHANDL), IERR)                             Md
      ENDIF                                                             Md
C
C
      LINTAB = .FALSE.
C
C     Decode the Pathname
      MHASH = IFLTAB(KHASH)
C
C     Is this a collections record?
C     Collections are identified by an F part of /C:00000|REST OF FPART/
C     Where 00000 is generally a sequence number, for example
C     /YUBA/SMARTSVILLE/FLOW/01JAN1997/1HOUR/C:00042|OPERATION A/
C     We'll replace 00000 with XXXXX so that all records within a collection
C     have the same hash code, and then we only have to search that one hash
      CALL zcolpath6 (CKPATH, NPATH, CLPATH, LCOLL)
      IF (LCOLL) THEN
C        If we are writing a collection the first time for this file,
C        indicate so in the perm area
         IF (LWRITE) THEN
            IF (IFLTAB(KCOLL).EQ.0) THEN
                IFLTAB(KCOLL) = 1
C               Since we are in a write mode, the file's already locked
                NADD = 1
                CALL zptrec6(IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
            ENDIF
         ELSE
            IF (IFLTAB(KCOLL).EQ.0) THEN
                IF (IFLTAB(KLOCK).EQ.1) THEN
                    CALL zmultu6 ( IFLTAB, .TRUE., .TRUE.)
                    CALL zrdprm6(IFLTAB, .TRUE.)
                    IFLTAB(KCOLL) = 1
                    CALL zmultu6( IFLTAB, .FALSE., .TRUE.)
                ELSE
                    IFLTAB(KCOLL) = 1
                    NADD = 1
                    CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM,
     *                           NADD, .TRUE.)
C                   Dump the buffer to disk
                    CALL zbdump6(IFLTAB, 1)
                ENDIF
            ENDIF
         ENDIF
         CALL zhash6 (CLPATH, NPATH, MHASH, IHASH)
         IF (MLEVEL.GE.12) WRITE (MUNIT,44) IHASH, CLPATH(1:NPATH)
 44      FORMAT (T8,'Collections path, collection hash code: ',I5,
     *         /,T8,'Collection Representation Pathname: ',A)
      ELSE
         CALL zhash6 (CKPATH, NPATH, MHASH, IHASH)
      ENDIF
C
C     If a Hash table is used, read the address for this hash code
      IF (IFLTAB(KTABLE).EQ.1) THEN
      IADD = NPERM + IHASH
      CALL zgtrec6 (IFLTAB, IPBADD, 1, IADD, .FALSE.)
      IF (MLEVEL.GE.10) WRITE (MUNIT,45) IADD, IPBADD
 45   FORMAT (T12,'Hash Table address:',I8,'  Bin address:',I8)
C     Does a pathname bin exist for this hash code?  (Exit if no)
      IF (IPBADD.EQ.0) GO TO 90
C
      ELSE IF (IFLTAB(KTABLE).EQ.2) THEN
C
C     If no Hash table is used (type 2), read the bin directly
      IPBADD = ((IHASH - 1) * IFLTAB(KBNSIZ)) + IFLTAB(KAFBIN)
C
      ELSE
C     Should not get here
      WRITE (MUNIT,*)'IFLTAB CORRUPT IN zcheck6, KTABLE DOES NOT MATCH',
     *  IFLTAB(KTABLE)
      CALL zabort6 (IFLTAB, 100, 'zcheck6', 0, IFLTAB(KTABLE), ' ')
      RETURN
      ENDIF
C
C     This Hash Code is in the table
      LINTAB = .TRUE.
 60   CONTINUE
C
C     Read the pathname bin
      ISIZE = IFLTAB(KBNSIZ)
C     Check to see if the file size has increased since last read
C     and this read is beyond what we think the file size is.
      IF ((IPBADD.GT.IFLTAB(KFSIZE)).AND.(.NOT.LWRITE))
     *  CALL zrdprm6 (IFLTAB, .TRUE.)
      CALL zgtrec6 (IFLTAB, IPNBIN, ISIZE, IPBADD, .FALSE.)
C     Did we get an error on the last read?
      IF (IFLTAB(KSTAT).NE.0) THEN
         GO TO 100
      ENDIF
      JPNBIN = 1
C
C     Loop through the pathname bin, looking for this pathname
 80   CONTINUE
C     Any more pathnames left?
      IF (IPNBIN(JPNBIN).EQ.0) GO TO 90
C
C     Yes - See if it is the correct one.
      IPLOOP = IPLOOP + 1
      NBWPAT = ((IPNBIN(JPNBIN+KBNPAT)-1) / NCPW) + 1
      IPSTAT = IPNBIN(JPNBIN)
      IF (IPSTAT.GT.10) IPSTAT = IPSTAT - 10
      IF ((IPSTAT.EQ.1).OR.(LUNDEL.AND.(IPSTAT.EQ.2)))
     * THEN
C
C     Check the length of the pathname
      IF (NPATH.EQ.IPNBIN(JPNBIN+KBNPAT)) THEN
C     Same length - check the pathnames
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CLPATH, NPMWRD)
      IF (CKPATH(1:NPATH).EQ.CLPATH(1:NPATH)) THEN
C     Same Pathnames - record found.  Report NHEAD and NDATA
      LFOUND = .TRUE.
      NHEAD = IPNBIN(JPNBIN+KBNHEA+NBWPAT)
      NDATA = IPNBIN(JPNBIN+KBNDAT+NBWPAT)
C     Save this pathname's name and location
      IFLTAB(KPADD) = IPBADD
      IFLTAB(KPJBIN) = JPNBIN
      IFLTAB(KAINFO) = IPNBIN(JPNBIN+KBAINF+NBWPAT)
      IFLTAB(KDTYPE) = IPNBIN(JPNBIN+KBTYPE+NBWPAT)
      IFLTAB(KPNHEA) = NHEAD
      IFLTAB(KPNDAT) = NDATA
      IFLTAB(KLPFOU) = 1
      IFLTAB(KINTAB) = 1
      IFLTAB(KLPATL) = NPATH
      CALL CH2HOL ( CKPATH, IFLTAB(KLPATH), NPMWRD)
C
C     Check to see if the file size has increased since last read
C     and this read is beyond what we think the file size is.
      IF ((IFLTAB(KAINFO).GT.IFLTAB(KFSIZE)).AND.(.NOT.LWRITE))
     *  CALL zrdprm6 (IFLTAB, .TRUE.)
C
      GO TO 100
      ENDIF
      ENDIF
C
C     Is this the same record, and it was deleted earlier??
      ELSE IF (((IPNBIN(JPNBIN).EQ.2).OR.(IPNBIN(JPNBIN).EQ.12))
     *  .AND.(LWRITE)) THEN
C     Check the length of the pathname
      IF (NPATH.EQ.IPNBIN(JPNBIN+KBNPAT)) THEN
C     Same length - check the pathnames
      CALL HOL2CH (IPNBIN(JPNBIN+KBPATH), CLPATH, NPMWRD)
      IF (CKPATH(1:NPATH).EQ.CLPATH(1:NPATH)) THEN
C     Yes - re-mark record as unaccessible!
      IPNBIN(JPNBIN) = IPNBIN(JPNBIN) + 2
      CALL zptrec6 (IFLTAB, IPNBIN, ISIZE, IPBADD, .FALSE.)
      CALL zgtrec6 (IFLTAB, INFO, 2, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
      IF ((INFO(KIFLAG).EQ.NPFLAG).AND.(INFO(KISTAT).EQ.2)) THEN
      INFO(KISTAT) = IPNBIN(JPNBIN)
      CALL zptrec6 (IFLTAB, INFO, 2, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
      ENDIF
      ENDIF
      ENDIF
C
      ELSE IF (IPNBIN(JPNBIN).EQ.-1) THEN
C     No more paths in this bin (bin full) - go to next path bin
      N = IFLTAB(KBNSIZ) - 1
      IPBADD = IPNBIN(N)
      GO TO 60
      ENDIF
C     Check next path in block (unless no more)
      JPNBIN = JPNBIN + NBWPAT + NLBIN
      IF (JPNBIN.GT.(IFLTAB(KBNSIZ)-2)) GO TO 90
      GO TO 80
C
C
C     Record Not Found - remember Pathname
 90   CONTINUE
      IFLTAB(KLPFOU) = 0
      IF (LINTAB) THEN
      IFLTAB(KINTAB) = 1
      ELSE
      IFLTAB(KINTAB) = 0
      ENDIF
      IFLTAB(KLPATL) = NPATH
      CALL CH2HOL ( CKPATH, IFLTAB(KLPATH), NPMWRD)
C
 100  CONTINUE
      IF (MLEVEL.GE.10) WRITE (MUNIT,120) IFLTAB(KUNIT), LFOUND, LINTAB
 120  FORMAT (T8,'-----DSS---Debug:  Exit zcheck6',/,T10,'Unit:',I5,
     * '  Found: ',L1,'  In table: ',L1)
C
      RETURN
C
 900  CONTINUE
      WRITE (MUNIT,901)
 901  FORMAT (/,' -----DSS---zcheck6:  **** ERROR;  Uncompleted',
     * ' Buffered Write ****',/,' (All Buffered Writes MUST be',
     * ' Completed prior to any other DSS call.)',/)
      CALL zabort6 (IFLTAB, 60, 'zcheck6', 0, IFLTAB(KUNIT), ' ')
      RETURN
C
      END

