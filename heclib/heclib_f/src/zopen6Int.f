      SUBROUTINE zopen6Int ( IFLTAB, CFNAME, ISTAT)
C
C
C     Open DSS file called CFNAME
C     The file will be generated if not existing
C     Initalizes DSS software, and sets up DSS file
C     structure in CFNAME
C
C     DSS File Structure Level 6:  Written by Bill Charley and
C                                  Art Pabst, HEC 1988
C
C
      INTEGER IFLTAB(*)
C
      COMMON /WORDS/ IWORD(10)
      INTEGER JSTAT
C
      CHARACTER CFNAME*(*), CTEMP*256
      CHARACTER CNAME*256
      CHARACTER C7VERS*8
C
      LOGICAL LNEW, LEXIST
      INTEGER INUMB
C     LOGICAL LOP                                                       F
C     SAVE LFIRST
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdssmz.h'
C
      INTEGER(8) ISIZE8, IFSIZE8, IVAL8
C
      COMMON /TST /NUMBREADS
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
      DATA IHANDL /0/
C
C
      NUMBREADS = 0
C
      IF (MLEVEL.GE.10) WRITE ( MUNIT, 10) CFNAME
 10   FORMAT (/,T3,'-----DSS---Debug:  Enter ZOPEN6',/,
     * T8,'File Name Provided: ',A)
C
C     If this is the first access to a DSS subroutine, initialize
      IF (LFIRST) CALL zinit6
      LFIRST = .FALSE.
C
C     KSWAP sets a flag to indicate if all bytes should be swapped
C     They are swapped if the file is of the other style endian
C     (e.g., big endian vs. little endian).
C     If 0, then same endian (no swap), if 1, other endian (swap)
C     For now, little endian is used for all files (PC compatiable)
C     (thus always swapped on sun, mips).
      IFLTAB(KSWAP) = IWORD(4)
C
      IFLTAB(KSTAT) = 0
C
C     Add any file name extension and determine if the file exists.
      CNAME = ' '
      CALL zfname6 (CFNAME, CNAME, ILEN, LEXIST)
C
      IF (ILEN.EQ.0) THEN
      WRITE (MUNIT,20)
 20   FORMAT(/,T3,'-----DSS*** ZOPEN ERROR:  No File Name Specified!'/)
      ISTAT = -10
      GO TO 910
      ENDIF
C
      ISTAT = 0
      IFLTAB(KREADO) = 0
      LNEW = .NOT. LEXIST
C
      IF (MLEVEL.GE.10) WRITE ( MUNIT, 40) LEXIST, CNAME(1:ILEN)
 40   FORMAT (/,T3,'-----DSS---Debug:  ZOPEN;  Exist: ',L1,
     * ';  File: ',A)
C
C
      CALL WHEN (CDATE,CTIME)
      CALL zquery7('vers', C7VERS, INUMB)
      CALL CHRLNB(C7VERS, N7VERS)
C
C     Check if the unit is defined by a zset6 call
 50   CONTINUE
C
C     See if this unit number is already being used
C     (where FORTRAN I/O is used only).
C     INQUIRE (UNIT=IUNIT, OPENED=LOP)                                  F
C     IF (LOP) THEN                                                     F
C     IF (IUNIT.LT.200) GO TO 50                                        F
C     ENDIF                                                             F
C
C
C
C     Set this file type for multiple user access
C     IF IFLTAB(KMULT) = 0,  Exclusive open / Single user only mode
C        (The file was opened so that no one else can access the file)
C     IF IFLTAB(KMULT) = 1,  Read only mode, but locks are supported
C        (someone else can lock the file)
C     IF IFLTAB(KMULT) = 2,  Standard multi-user mode with locks
C     IF IFLTAB(KMULT) = 3,  Multi-user "advisory".  This locks the
C        file on the first write and only un-locks on close, or if
C        someone else requests access (where the mode goes to 2).
C        This is much faster if we might be the only one using the file.
C     IF IFLTAB(KMULT) = 4, Exclusive access with locks for WRITING only
C     IF IFLTAB(KMULT) = 5, Exclusive access with locks for both reading
C        and writing.  NOT ACTIVE!!!
C     IF IFLTAB(KMULT) = 6,  Release exclusive access (internal flag)
C
      IF (LEXCL) THEN
         IFLTAB(KMULT) = 4
      ELSE IF (LREADO) THEN
         IFLTAB(KMULT) = 1
         IFLTAB(KREADO) = 1
      ELSE
         IFLTAB(KMULT) = 3
      ENDIF
C
C     Open the file
C     OPEN (UNIT=IUNIT,ACCESS='DIRECT',RECL=NRECL,FILE=CNAME,           F
C    * IOSTAT=ISTAT)                                                    F
C
C
C
C
      IF (CNAME(ILEN:ILEN).EQ.'"') THEN
         ILEN = ILEN - 1
         CNAME(1:ILEN) = CNAME(2:ILEN)
         ILEN = ILEN -1
      ENDIF
      CNAME(ILEN+1:ILEN+1) = CHAR(0)
      CALL openf ( CNAME(1:ILEN+1), 10, IHANDL, ISTAT)
      IF (ISTAT.NE.0) THEN
      CALL openf ( CNAME(1:ILEN+1), 0, IHANDL, ISTAT)
      IF (ISTAT.EQ.0) THEN
      IFLTAB(KREADO) = 1
      IFLTAB(KMULT)  = 1
      ENDIF
      ENDIF
C     Check to see if this file is on the local disk or is a remote one.
      CALL isonlocaldrive (CNAME(1:ILEN+1), ILOCAL)                     M
      IF (ILOCAL.EQ.0) IFLTAB(KREMOTE) = 1                              M
      IUNIT = IHANDL
      CNAME(ILEN+1:ILEN+1) = ' '
C
 80   CONTINUE
      IF (MLEVEL.GE.11) WRITE (MUNIT, 100) ISTAT, IUNIT, LNEW, NRECL,
     * IFLTAB(KMULT), LMULTU, CNAME(1:ILEN)
 100  FORMAT (T7,'Open status:',I5,',  Unit:',I5,',  New:',L2,
     * ',  Record Length:',I5,' Bytes',
     * /,T7,'Multiple User Access:',I4,',  Config request:',L2,
     * /,T7,'Name: ',A)
C
      IF (MLEVEL.GE.11) WRITE (MUNIT, *)' Handle: ',IHANDL
C
      IF (ISTAT.NE.0) THEN
      WRITE ( MUNIT, 120) CNAME, ISTAT
 120  FORMAT (/,' -----DSS*** ZOPEN:  Error - Unable to Open File: ',A,/
     * '    Open Status:',I8,/)
      GO TO 800
      ENDIF
C
 140  CONTINUE
C
C     Save File structure verion in file table
      IFLTAB(KNV) = 6
      IFLTAB(KUNIT) = IUNIT
      IFLTAB(KHANDL) = IHANDL                                           MdLu
C     IFLTAB(KHANDL) = IUNIT                                            F
      NFILES = NFILES + 1
      IFLTAB(KFILES) = NFILES
C
 145  CONTINUE
      CALL CH2HOL (CNAME, IFLTAB(KNAME), 16)
      IFLTAB(KFILEW) = 0
C
C     Set KEY elements in IFLTAB (to insure that it
C     does not become corrupt)
C
      IFLTAB(KEY1) = NKEY
      IFLTAB(KEY2) = NKEY
      IFLTAB(KEY3) = NKEY
C
C
C     Initialize buffer parameters
C
      IFLTAB(KLOCK) = 0
      IFLTAB(KLOCKR) = 0
      IFLTAB(KWLOCK) = 0
      IFLTAB(KFSIZE) = 0
      IFLTAB(KLPATL) = 0
      CALL zbdump6 (IFLTAB, 1)
C
C
C     Get permanent section of file (if it exists)
C
      IF (.NOT.LNEW) THEN
      CALL zrdprm6 (IFLTAB, .FALSE.)
C
C     Was this file not closed with write lock on last time?
      IF (IFLTAB(KFSIZE).EQ.-1357) THEN
      IF (MLEVEL.GE.1) WRITE (MUNIT, 150) CNAME(1:ILEN)
 150  FORMAT (/,' *****DSS***ZOPEN:  File was not closed during a',
     * ' Locked Access',/,' The file size is unknown;  Name: ',A,/,
     * ' --- FILE ACCESS IS READ ONLY ---',/,
     * ' Squeeze the file with DSSUTL to correct this condition.')
C     Set in a read only state (so we can at least squeeze the file)
      IFLTAB(KREADO) = 1
C     Get the best estimate of the file size.
C     CNAME(ILEN+1:ILEN+1) = CHAR(0)                                    u
C     CALL FILESIZEN (CNAME(1:ILEN+1), ISIZE, JSTAT)                    u
C     CNAME(ILEN+1:ILEN+1) = ' '                                        u
      CALL filesize64 (IFLTAB(KHANDL), ISIZE8)                          Md
      IFSIZE8 = ((ISIZE8 - 1)/ 4) + 1
      IF (IFSIZE8.LT.100) IFSIZE8 = 9999999
      IFLTAB(KFSIZE) = CONVI8TOI4(IFSIZE8)
      ENDIF
C
      ENDIF
C
C     Check if this is a DSS file
      CALL HOLCHR (IFLTAB(KDSS), 1, NDSSC, CTEMP, 1)
      IF (MLEVEL.GE.11) WRITE (MUNIT, 155) CTEMP(1:NDSSC),CDSS(1:NDSSC),
     * C7VERS(1:N7VERS)
 155  FORMAT (T11,'File Identifier Read -',A,'-  Current identifier -',
     * A,'-', ', Library ', A)
C
      IF (LNEW) CTEMP = ' '
      IF (CTEMP(1:NDSSC).EQ.CDSS(1:NDSSC)) THEN
C     Yes it is a DSS file -
C     see if it is the current software version
      CALL HOLCHR (IFLTAB(KVERS), 1, NVERSC, CTEMP, 1)
      IF(MLEVEL.GE.11)WRITE(MUNIT,156)CTEMP(1:NVERSC), CVERS(1:NVERSC)
 156  FORMAT (T11,'File Version Read -',A,'-  Current Version -',A,'-')
C
C     Get an integer representation of the file version
C     For version 6-AB, this number is 100 * (ICHAR(A)) + ICHAR(B)
      IFLTAB(KVERNO) = 60000+ 100*(ICHAR(CTEMP(3:3)))+ICHAR(CTEMP(4:4))
C
C
C     Is this a vaild DSS file?
      IF (CTEMP(1:2).EQ.CVERS(1:2)) THEN
C
C     Check if different endian machine (bytes swapped)
         IF (IFLTAB(KTABLE).GT.10000) THEN
            CALL zswap6 (IFLTAB(KTABLE), NADD)
            IF ((NADD.EQ.1).OR.(NADD.EQ.2)) THEN
            IF (IFLTAB(KSWAP).EQ.0) THEN
               IFLTAB(KSWAP) = 1
            ELSE
               IFLTAB(KSWAP) = 0
            ENDIF
            GO TO 145
            ENDIF
         ENDIF
C
C     Check that we have a vaild file size
         IF (IFLTAB(KFSIZE).LT.0) THEN
            WRITE (MUNIT, 158) CNAME(1:ILEN)
 158        FORMAT (/,' ----DSS---ZOPEN;  ERROR:  Invalid file size',
     *              /,' Unable to open file, Name: ',A)
            ISTAT = -2
            GO TO 900
         ENDIF
C
C        Use the record following the permenant section as the
C        one record to lock to indicate the file is locked
         IF (IFLTAB(KTABLE).EQ.1) THEN
            ISIZE = IFLTAB(KHASH) + NPERM + 1
         ELSE
            ISIZE = NPERM + 1
         ENDIF
         CALL zgetrw6 (ISIZE, IREC, IWRD)
         IF (IWRD.NE.1) IREC = IREC + 1
         IFLTAB(KLOCKB) = (IREC - 1) * 512                              Mdu
C
C     Check that the file is not exclusively locked
*****************FIX ME HERE ***********************
         IF (IFLTAB(KMULT).GT.0) THEN
            ISIZE = IFLTAB(KLOCKB) + (IWORD(2) * 2)
            CALL lockdss (IFLTAB(KHANDL), 3, ISIZE, IWORD(2), IERR)
            IF (IERR.NE.0) THEN
               IF (MLEVEL.GE.1) WRITE (MUNIT,159) CNAME(1:ILEN)
 159           FORMAT (T5,'-----DSS---ZOPEN:  ERROR:  DSS file is ',
     *         'currently exclusively locked',/,T5,'DSS file: ',A)
               ISTAT = -5
               GO TO 900
            ENDIF
         ENDIF
C
C
C     It is a vaild DSS file
      IF (MLEVEL.GT.1) WRITE (MUNIT,160) CNAME(1:ILEN), IUNIT,
     * CVERS(1:NVERSC), CTEMP(1:NVERSC), C7VERS(1:N7VERS)
 160  FORMAT (T5,'-----DSS---ZOPEN:  Existing File Opened,  File: ',A,/,
     * T24,'Unit:',I5,';  DSS Versions - Software: ',A,', File: ',A,
     * ',  Library ', A)
      IF ((IFLTAB(KREADO).EQ.1).AND.(MLEVEL.GE.2)) WRITE (MUNIT, 180)
 180  FORMAT (T24, '(Read Access Only)')
      IF ((MLEVEL.GE.2).AND.((LSTABL).OR.(LSZSET))) WRITE (MUNIT, 190)
 190  FORMAT (T5,'-----DSS--- Parameters Set for Existing File Ignored')
C
      NADD = IFLTAB(KFSIZE) - 1
      CALL zgetrw6 (NADD, IREC, IWRD)
      IFLTAB(KMXREC) = IREC
C
C
      ELSE
      WRITE ( MUNIT, 200) CTEMP(1:NVERSC), CVERS(1:NVERSC)
      ISTAT = -3
      GO TO 900
      ENDIF
C
C
      ELSE
C
C
C     Check for older software (incompatiable) versions
      IF (.NOT.LNEW) THEN
      CALL HOLCHR (IFLTAB(KVERS), 1, NVERSC, CTEMP, 1)
      IF (CTEMP(2:2).EQ.'-') THEN
      WRITE ( MUNIT, 200) CTEMP(1:NVERSC), CVERS(1:NVERSC)
 200  FORMAT (//,' ***** DSS: Incompatible DSS Software Versions *****'
     * ,/,' The File Specified is Version ',A,/
     * ' This Software is Version ',A,/
     * ' These Versions are Incompatible - Please Update',
     * ' Your DSS File, or this Program',//)
      ISTAT = -3
      GO TO 900
      ENDIF
      ENDIF
C
C
C     New file - Be sure we are not read only
      IF (IFLTAB(KREADO).EQ.1) THEN
      WRITE (MUNIT, 210) CNAME(1:ILEN)
 210  FORMAT (/,' ----DSS---ZOPEN;  ERROR:  Read Access Only Given',
     * ' for New File',/,' Unable to open file, Name: ',A)
      ISTAT = -2
      GO TO 900
      ENDIF
C
C
C     Be sure this is not someone else's file that will be written over
C     CNAME(ILEN+1:ILEN+1) = CHAR(0)                                    u
C     CALL FILESIZEN (CNAME(1:ILEN+1), ISIZE, JSTAT)                    u
C     CNAME(ILEN+1:ILEN+1) = ' '                                        u
      CALL filesize64 (IFLTAB(KHANDL), ISIZE8)                          Md
      IF (ISIZE8.GT.1000) THEN
         WRITE (MUNIT, 215) CNAME(1:ILEN)
 215     FORMAT (/,' ----DSS---ZOPEN;  ERROR:  Existing file is not',
     *    ' a DSS File',/,' Unable to open file, Name: ',A)
         ISTAT = -6
         GO TO 900
      ENDIF
C
C
C
C     Create permanent section
C
      K = KPERM + NPERM - 1
      DO 220 I=KPERM,K
      IFLTAB(I) = 0
 220  CONTINUE
C
C     Write the zeroed permanent section to disk
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
C
C     Set 'ZDSS' at the first location in the file
      CALL CHRHOL (CDSS, 1, NDSSC, IFLTAB(KDSS), 1)
      IFLTAB(KNRECS) = 0
      IFLTAB(KSEQNO) = 0
      IFLTAB(KDEAD) = 0
      IFLTAB(KMXREC) = 0
      CALL CHRHOL (CVERS, 1, NVERSC, IFLTAB(KVERS), 1)
C
C     Get an integer representation of the file version
C     For version 6-AB, this number is 100 * (ICHAR(A)) + ICHAR(B)
      IFLTAB(KVERNO) = 60000+ 100*(ICHAR(CTEMP(3:3)))+ICHAR(CTEMP(4:4))
C
C     If the file is new because it is being squeezed, keep
C     the old file date.
      IF (LFDATE) THEN
      CALL CHRHOL (CFDATE, 1, NDATEC, IFLTAB(KCREAT), 1)
      LFDATE = .FALSE.
      ELSE
      CALL CHRHOL (CDATE, 1, NDATEC, IFLTAB(KCREAT), 1)
      ENDIF
C
C     Save the last written date and time in the perm section
      CALL CHRHOL (CDATE, 1, NDATEC, IFLTAB(KLWDAT), 1)
      CALL CHRHOL (CTIME, 1, NTIMEC, IFLTAB(KLWTIM), 1)
C
C     Set the file size, and structure (via zset6 or default)
      CALL zfsize6 (IFLTAB)
C
C     If this structre uses a hash table, set aside space for it
      IF (IFLTAB(KTABLE).EQ.1) THEN
      IFLTAB(KBNREM) = IFLTAB(KBNBLK)
      IFLTAB(KAFBIN) = IFLTAB(KHASH) + NPERM + 1
C
C     Set aside space for the first set of pathname bins
C     Keep the path bins on record boundaries
C     This will waste a little space, but improve speed significantly
      CALL zgetrw6 (IFLTAB(KAFBIN), JREC, IWRD)
      IF (IWRD.NE.1) JREC = JREC + 1
C     Add an additional record to use for locking
      IFLTAB(KLOCKB) = (JREC - 1) * 512                                 Mdu
      CALL zgetad6 (ISIZE, JREC, 1)
C     Also save the lock record on disk
      CALL zptrec6 (IFLTAB, 0, 1, ISIZE, .FALSE.)
      JREC = JREC + 1
      CALL zgetad6 (IFLTAB(KAFBIN), JREC, 1)
C
      IFLTAB(KANBIN) = IFLTAB(KAFBIN)
      IFLTAB(KFSIZE) = IFLTAB(KAFBIN)
      ISIZE = IFLTAB(KBNBLK) * IFLTAB(KBNSIZ)
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + ISIZE
C
      ELSE
C
C     This structrue does not use a hash table.  The has points
C     directly to the first pathname bin for that has code.
C     Set aside space equal to the hash size time the bin size
      IFLTAB(KBNREM) = 0
      IFLTAB(KAFBIN) = NPERM + 1
C
C     Keep the path bins on record boundaries
C     This will waste a little space, but improve speed significantly
      CALL zgetrw6 (IFLTAB(KAFBIN), JREC, IWRD)
      IF (IWRD.NE.1) JREC = JREC + 1
C     Add an additional record to use for locking
      IFLTAB(KLOCKB) = (JREC - 1) * 512                                 Mdu
      CALL zgetad6 (ISIZE, JREC, 1)
C     Also save the lock record on disk
      CALL zptrec6 (IFLTAB, 0, 1, ISIZE, .FALSE.)
      JREC = JREC + 1
      CALL zgetad6 (IFLTAB(KAFBIN), JREC, 1)
C
      IFLTAB(KANBIN) = IFLTAB(KAFBIN)
      ISIZE = (IFLTAB(KHASH) - 1) * IFLTAB(KBNSIZ)
      IFLTAB(KANBIN) = IFLTAB(KANBIN) + ISIZE
      IFLTAB(KFSIZE) = IFLTAB(KAFBIN)
      ISIZE = IFLTAB(KHASH) * IFLTAB(KBNSIZ)
      IFLTAB(KFSIZE) = IFLTAB(KFSIZE) + ISIZE
      ENDIF
C
C     Iniitalize efficiency variables
      IFLTAB(KBINS)  = 0
      IFLTAB(KHUSED) = 0
      IFLTAB(KBOVER) = 0
      IFLTAB(KMAXPH) = 0
C
      IFLTAB(KCOMPI) = 0
      IFLTAB(KCOMPN) = 0
C
      IFLTAB(KITSIN) = 100
      IFLTAB(KITSDA) = 200
      IFLTAB(KITSMO) = 200
      IFLTAB(KITSYE) = 200
      IFLTAB(KITSDE) = 200
      IFLTAB(KITSCE) = 200
C
      IFLTAB(KCOLL)  = 0
C
C     Write permanent section to the file with zero file size
C     This prevents reads for zptrec6, and protects the file in
C     case an abort occurs before the open is complete
      IFSIZE = IFLTAB(KFSIZE)
      IFLTAB(KFSIZE) = 0
      IFLTAB(KBSADD) = 0
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
C
C     Dump the buffer to disk
      N = JBUFF
      CALL zbdump6 (IFLTAB, 1)
      JBUFF = N
      JCREC(JBUFF) = 1
      LSBUFF(JBUFF) = .TRUE.
      IFLTAB(KFSIZE) = IFSIZE
C
      IF (IFLTAB(KTABLE).EQ.1) THEN
C     Write a zerored Hash Table
      NADD = NADD + NPERM
      JSIZE = -(IFLTAB(KHASH))
      CALL zptrec6 (IFLTAB, IZERO,  JSIZE, NADD, .FALSE.)
C
C     Write the first pathname bin block
      ISIZE = IFLTAB(KBNBLK) * IFLTAB(KBNSIZ)
      CALL zptrec6 (IFLTAB, ISIZE, -1, IFLTAB(KAFBIN), .FALSE.)
C
      ELSE
C     This is a structure 2 type file -
C     Write all pathanme bins to the file (takes a while!)
      ISIZE = IFLTAB(KHASH) * IFLTAB(KBNSIZ)
      CALL zptrec6 (IFLTAB, ISIZE, -1, IFLTAB(KAFBIN), .FALSE.)
      ENDIF
C
      IFLTAB(KFILEW) = 1
C
C     Re-write permanent section to the file with correct file size
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
C
C
C     Flush all buffers to disk
      CALL zbdump6 (IFLTAB, 1)
C

      IF (MLEVEL.GE.1) WRITE (MUNIT,240) CNAME(1:ILEN), IUNIT,
     * CVERS(1:NVERSC), C7VERS(1:N7VERS)
 240  FORMAT (T5,'-----DSS---ZOPEN:  New File Opened,  File: ',A,/,
     * T24,'Unit:',I5,';  DSS Version: ',A, '  Library ',A)
C
      ENDIF
C
C     If we are in an exclusive lock mode, lock the entire file
      IF ((IFLTAB(KMULT).EQ.4).OR.(IFLTAB(KMULT).EQ.0)) THEN
         CALL zmultu6 (IFLTAB, .TRUE., .TRUE.)
         IF (MLEVEL.GE.1) WRITE (MUNIT,260)
 260     FORMAT (T24, '(Exclusive Access Only)')
      ENDIF
C
C
C
 800  CONTINUE
C
C     Determine if we have to swap words for double precsion values
C     on big endian machines to keep compatitable with little endians
      if (ifltab(kverno).eq.63232) then
          ! file version = "6-  " (i.e., new file)
          iversion = 60000+ 100*(ichar(cvers(3:3)))+ichar(cvers(4:4))
	  else
          iversion = ifltab(kverno)
	  end if
      IF((IFLTAB(KSWAP).NE.0).AND.(IVERSION.GE.67900).OR.
     * (IFLTAB(KNV).GT.6))  THEN
          IFLTAB(KDSWAP) = 1
          IF(MLEVEL.GE.10)WRITE(MUNIT,*)'Double precision swap set on'
      ELSE
          IFLTAB(KDSWAP) = 0
      ENDIF
C
C     If this file has been set in a write lock mode,
C     read in physical records 1 and 2, then lock them.
C
      IF (LWLOCK) THEN
C     Be sure that we have no more than two records locked at this time
C     (from other file(s))
      J = 0
      DO 820 I=1,MXBUFF
      IF (LOCKBF(I)) J = J + 1
 820  CONTINUE
C
      IF (J.LE.2) THEN
C     Write to the file a flag indicating Write Lock on,
C     in case there is an abort, we will know the file size is unknown!!
      IFSIZE = IFLTAB(KFSIZE)
      IFLTAB(KFSIZE) = -1357
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .FALSE.)
      CALL zbdump6 (IFLTAB, 1)
      IFLTAB(KFSIZE) = IFSIZE
C     Now lock records 1 and 2
      NADD = 1
      CALL zptrec6 (IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
      JREC = 2
      CALL zgetad6 (NADD, JREC, 1)
      CALL zgtrec6 (IFLTAB, INFO, 1, NADD, .TRUE.)
      LOCKBF(JBUFF) = .TRUE.
      IFLTAB(KWLOCK) = 1
      ENDIF
C
      ENDIF
C
C
      LSZSET = .FALSE.
      LSTABL = .FALSE.
      LEXCL = .FALSE.
      LWLOCK = .FALSE.
      LREADO = .FALSE.
      NSIZE = 0
      IHSIZE = 0
      IFLTAB(KCOMP) = 0
      IFLTAB(KBSADD) = IFLTAB(KFSIZE)
      CSIZE = ' '
      IFLTAB(KLOCKRD) = 0
      CALL ZLOCKRD(IFLTAB, 1, JSTAT)
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT, 850) IUNIT, ISTAT
 850  FORMAT(T3,'-----DSS---Debug; Exit ZOPEN; Unit: ',I4,'Status:',I4)
C      CALL FLUSH(MUNIT)  
C
      RETURN
C
  900 CONTINUE
C     Error occured - Close the file
C     CLOSE (UNIT=IUNIT)                                                F
      CALL closf (IHANDL, JSTAT)                                        Mdu
 910  CONTINUE
      IFLTAB(KSTAT) = ISTAT
      IF (MLEVEL.GE.10) WRITE (MUNIT, 850) ISTAT
      RETURN
      END

