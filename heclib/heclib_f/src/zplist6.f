      SUBROUTINE zplist6 (IFLTAB, CINSTR, IFPOS, CPATH, NPATH, ISTAT)
C
C
C     Get all pathnames in a DSS file - return one at a time.
C     To start list, set IFPOS to 0.  At the end of the list
C     ISTAT will be set to 1 (with no pathname returned), otherwise
C     ISTAT will be 0 for a normal return with pathname, or <0 for
C     an (somewhat serious) error return.
C
C     CINSTR is a string with instructions on how to perform this
C     operation.  It can be either a selective catalog command
C     (e.g., 'B=NORTH FORK'), or specify what kind of records.
C     Valid entries include:
C     'DELETE'
C     'RENAME'
C   ****** LW not implemented yet *****
C     'LW>date time'  (Last Written later than DATE TIME, where date is
C     'LW<date time'   9 char mil style, and time may include seconds.
C     'LW=date time'   e.g., 'LW>04JUN2004 14:00:00')
C     'PR=program name' (program is the name stored with the record)
C     'S=string'  (Pathname contains this string)
C     'A=..., B=...' (Standard selective catalog)
C
C      These items cannot be combined (cannot have 'DELETE, LW>date')
C
C
C
C     Written by Bill Charley at HEC.
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
      CHARACTER CINSTR*(*), CPATH*(*)
C
      INTEGER IPBIN(128)
      LOGICAL LSELCA, LMATCH
      CHARACTER CPDATE*10, CPPROG*10, CINST*100
      SAVE CINST
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      INTEGER ISELCA
C       ISELCA:  0 - No selective catalog,
C                1 - Scan (index) string only
C                2 - Full selective catalog
C
C
C
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFLTAB(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zplist6;  Unit:',I5)
C
C
C     Have we reached the end of the file
      IF (IFPOS.GE.IFLTAB(KFSIZE)) GO TO 850
C
C
C
      IF (IFPOS.LE.0) THEN
C        If this is the first entrance, set any selective catalog items
         ISELCA = 0
         CALL CHRLNB(CINSTR, NINST)
         IF (NINST.GT.0) THEN
            CINST = CINSTR
            CALL UPCASE(CINST)
            IF (CINST(1:3).EQ.'DEL') THEN
               ISELCA = 10
            ELSE IF (CINST(1:3).EQ.'REN') THEN
               ISELCA = 11
            ELSE IF (CINST(1:3).EQ.'PR=') THEN
               ISELCA = 3
               CINST = CINSTR(4:)
               NINST = NINST - 3
            ELSE IF (CINST(1:2).EQ.'LW') THEN
               ISELCA = 0
            ELSE IF (CINST(1:2).EQ.'S=') THEN
               ISELCA = 1
            ELSE
               CALL zsetca6 (CINST(1:NINST), LSELCA)
               IF (LSELCA) THEN
                  ISELCA = 2
                ENDIF
            ENDIF
         ENDIF
C
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
      LMATCH = .TRUE.
      IF ((IPBIN(JPBIN).EQ.1).OR.(IPBIN(JPBIN).EQ.11).OR.
     *    (ISELCA.GE.10)) THEN
C        Valid record.  Get pathname and pathname length
         CALL chrblk(CKPATH)
         NPATH = IPBIN(JPBIN+KBNPAT)
         IF (NPATH.LE.0) GO TO 180
         IF (NPATH.GT.390) GO TO 180
         CALL HOL2CH (IPBIN(JPBIN+KBPATH), CKPATH, NBMWPA)
C
         IF (MLEVEL.GE.12) WRITE(MUNIT, 141) CKPATH(1:NPATH)
 141     FORMAT(' Path found:         ',A)
 142     FORMAT(' Switched to Pseudo: ',A)
C        Should we show a pseudo regular interval pathname?
         IF (LPSEUDO.AND.(IPBIN(JPBIN+NBPWPA+KBPINTL).GT.0)) THEN
            CALL ZPseudoRTS6(CKPATH(1:NPATH), CPATH,
     *                 IPBIN(JPBIN+NBPWPA+KBPINTL), 2, ISTAT)
             IF (ISTAT.EQ.0) THEN
                CKPATH = CPATH
                CALL CHRLNB(CKPATH, NPATH)
                IF (MLEVEL.GE.12) WRITE(MUNIT, 142) CKPATH(1:NPATH)
             ENDIF
          ENDIF
C
C        Does it match any selective catalog parameters?
         IF (ISELCA.EQ.1) THEN
C           String index
            IF (INDEX(CKPATH(1:NPATH), CINST(1:NINST)).LT.1) THEN
               LMATCH = .FALSE.
            ENDIF
         ELSE IF (ISELCA.EQ.2) THEN
C           Selective catalog
            CALL zupath(CKPATH(1:NPATH), IBPART, IEPART, ILPART, ISTAT)
            CALL ZMATCA (CKPATH, IBPART, IEPART, ILPART,
     *                   CPDATE, CPPROG, LMATCH)
         ELSE IF (ISELCA.EQ.3) THEN
C           Program that wrote the record
*           CALL HOLCHR (INFO(NBPWPA+KIPROG), 1, NPROGC, CCPROG, 1)
         ELSE IF (ISELCA.EQ.10) THEN
C           Look for deleted (only) records
            IF ((IPBIN(JPBIN).NE.2).AND.(IPBIN(JPBIN).NE.12)) THEN
               LMATCH = .FALSE.
            ENDIF
         ELSE IF (ISELCA.EQ.11) THEN
C           Look for renamed (only) records
            IF ((IPBIN(JPBIN).NE.3).AND.(IPBIN(JPBIN).NE.13)) THEN
               LMATCH = .FALSE.
            ENDIF
         ENDIF
         IF (LMATCH) THEN
            call strcpy(CPATH, CKPATH)
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
 180  CONTINUE
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
 840  FORMAT (T6,'-----DSS---Debug:  Exit  zplist6')
      RETURN
C
C
 850  CONTINUE
C     End of file
      ISTAT = 1
      IFPOS = IFLTAB(KFSIZE)
      call CHRBLK(CPATH)
      NPATH = 0
      IFLTAB(KEY3+1) = 0
      IFLTAB(KEY3+2) = 0
      IFLTAB(KEY3+3) = 0
      GO TO 820
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE(MUNIT,920)
 920  FORMAT(' -----DSS---Error:  File error detected in zplist6')
C      CALL zerror6 (IFLTAB, 100, 'zplist6', 0, IFLTAB(KTABLE), ' ', 0,
C     * ' ', 0)
      IF (ISTAT.EQ.-2) THEN
      IF (MLEVEL.GE.1) WRITE(MUNIT,930) IADD
 930  FORMAT(' zplist6 Error: File address out of range:',I10)
      ELSE IF (ISTAT.EQ.-3) THEN
	IF (MLEVEL.GE.1) WRITE(MUNIT,930) IADD
      ELSE IF (ISTAT.EQ.-4) THEN
      IF (MLEVEL.GE.1) WRITE(MUNIT,940) IPBIN(JPBIN)
 940  FORMAT(' zplist6 Error: Pathname status out of range:',I10)
      ENDIF
      IF (ISTAT.GE.0) ISTAT = -1
      GO TO 820
C
      END

