      SUBROUTINE zckpdd6 (IFLTAB, INSTR, NFIXED, ISTAT)
C
C
C
C     Check for Paired Data double precision problems between
C     Big-Endian machines and little endian machines.
C
C     INSTR - Instructions (input)
C     0 - Not Valid.....   Just check DSS file version number
C     1 - Check DSS file version number and file, if old file
C     2 - Check DSS file, regardless of version number
C     3 - Update and fix file, if old version
C     4 - Update and fix file regardless of version (recommended for
C         Unix)
C
C     NFIXED - Number records that were or need to be fixed (output)
C
C     ISTAT - Status output
C     < 0 - Error (e.g., no write access)
C     0 - File okay as is (no update needed)
C     1 - File needs to be updated (call again with INSTR 3 or 4)
C     2 - Updated File Version
C     3 - Updated and Fixed file (there was a double problem).
C
C     Written by Bill Charley at HEC, 2006.
C
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssts.h'
C
      INTEGER IFLTAB(*), INSTR, ISTAT, JSTAT, NFIXED
      INTEGER IARRAY(NBSIZE)
      LOGICAL LUPDATE
      CHARACTER CPATH*400, CTEMP*10
      DOUBLE PRECISION DP
C
C
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,20) IFLTAB(KUNIT), INSTR
 20   FORMAT (T6,'-----DSS---Debug:  Enter ZCKPDD;  Unit:',I5,
     *        '  Instruction:', I5)
C
      ISTAT = 0
      JSTAT = 0
      NFIXED = 0
C
C     Determine if we have to swap words for double precsion values
C     on big endian machines to keep compatitable with little endians
*      IF((IFLTAB(KSWAP).NE.0).AND.((IFLTAB(KVERNO).GE.7900)).OR.
*     * (IFLTAB(KNV).GT.6))  THEN
*          IFLTAB(KDSWAP) = 1
C
C     Are we in a read only state?
      IF ((IFLTAB(KREADO).EQ.1).AND.(INSTR.GE.3)) GO TO 920
C
C    Lock the file
      IF (INSTR.GE.3) THEN
      CALL zmultu6(IFLTAB, .TRUE., .TRUE.)
      ENDIF
C
C     Is this version 6-OA or greater?  (100 * (ICHAR(O)) + ICHAR(A))
      IF (IFLTAB(KVERNO).LT.7900) JSTAT = 1

      IF (INSTR.EQ.0) THEN
         ISTAT = JSTAT
         GO TO 800
      ELSE IF (INSTR.EQ.1) THEN
         IF (JSTAT.EQ.0) GO TO 800
      ELSE IF (INSTR.EQ.3) THEN
         IF (JSTAT.EQ.0) GO TO 800
      ENDIF

C
C     Get the number of physical records in file (not data recs)
      IADD = IFLTAB(KFSIZE) - 1
      CALL ZGETRW6(IADD, NRECS, IW)
C
      DO 200 IREC=1,NRECS
C
C     Read physical record IREC from file
      CALL ZGETAD6(IADD, IREC, 1)
      CALL zgtrec6(IFLTAB, IARRAY, NBSIZE, IADD, .FALSE.)
C
C     Did we hit end of file?
      IF (IADD.LT.0) THEN
      WRITE (MUNIT, 28) IREC
 28   FORMAT (//, ' **** Caution:  Reached End of File at Physical',
     * ' Record:',I8/)
      GO TO 700
      ENDIF
C
C     Search through this record, looking for pathname flags
C
      DO 200 IWRD=1,NBSIZE
C
      IF (IARRAY(IWRD).EQ.NPFLAG) THEN
C
C     Found a flag - Get the first three words of the information
C     block to see if this indeed is the start of a data record
      CALL ZGETAD6(IADD, IREC, IWRD)
      CALL zgtrec6(IFLTAB, INFO, 3, IADD, .FALSE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 940
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,30)IADD, INFO(KISTAT), INFO(KINPAT)
 30   FORMAT (T5,'Found a INFO Flag at address',I13,/,
     * T5,'Status:',I5,';  Pathname Length:',I5)
      IPSTAT = INFO(KISTAT)
      IF (IPSTAT.GT.10) IPSTAT = IPSTAT - 10
C     Check for a valid status flag (1)
      IF (IPSTAT.EQ.1) THEN
C     Check for a valid pathname length
      NPATH = INFO(KINPAT)
      IF ((NPATH.GT.0).AND.(NPATH.LE.392)) THEN
C
C     Passed, therefore a valid record - Get the full information block
      NPPWRD = (NPATH - 1)/NCPW + 1
      NPMWRD = (NPATH - 1)/NCMW + 1
      CALL zgtrec6( IFLTAB, INFO, NINFO+NPPWRD, IADD, .FALSE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 940
C
C     Check for paired double data
      IF (INFO(NPPWRD+KITYPE).EQ.205) THEN
C     Get the pathname
      CPATH = ' '
      CALL HOLCHR (INFO(KIPATH), 1, NPATH, CPATH, 1)
C
C
      NDATA  = INFO(NPPWRD+KINDAT)
      IDADD  = INFO(NPPWRD+KIADAT)
      ITYPE  = INFO(NPPWRD+KITYPE)
      NLDATA = INFO(NPPWRD+KILNDA)
C
C     Check for addresses out of range.
      IF ((IDADD.LT.100).OR.(IDADD.GE.IFLTAB(KFSIZE))) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'Data address',
     *                                    IADD, IDADD, CPATH(1:NPATH)
 50   FORMAT(/,' -----DSS*** ZCKPDD;  Error: ',A,' out of Range',
     * /,' Unable to Copy Record;',/,' Info Block Address:',I12,
     * ',   Value:',I12,/,' Pathname: ',A)
      GO TO 200
      ENDIF
C
      IF (NDATA.GT.10000) THEN
C        Large data set, probably an error.  For now, only check a
C        frequent UNET error
      IF (NDATA.EQ.IDADD) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'Number of data values',
     *                                    IADD, NDATA, CPATH(1:NPATH)
      GO TO 200
         ENDIF
      ENDIF
C
C     Data Array
      IF (NDATA.GT.0) THEN
      NTOT = MIN0(NDATA,KLBUFF)
      IADD = INFO(NPPWRD+KIADAT)
      CALL zgtrec6(IFLTAB, ILBUFF, NTOT, IDADD, .FALSE.)
      NVAL = NTOT / 2
      IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DBUFF, NTOT)
      DP = 0.0
      NCOUNT = 0
      LUPDATE = .FALSE.
      DO 150 J=1,NVAL
         IF (DBUFF(J).NE.0.0) THEN
            DP = DABS(DLOG10(DABS(DBUFF(J)))) + DP
            NCOUNT = NCOUNT + 1
         ENDIF
 150  CONTINUE
      IF (NCOUNT.GT.0) THEN
         AVER = DP / NCOUNT
         IF (AVER.GT.20) LUPDATE = .TRUE.
      ENDIF
C
      IF (LUPDATE) THEN
         NFIXED = NFIXED + 1
         IF (INSTR.GE.3) THEN
            CALL ZDSWAP6(DBUFF, NTOT)
            CALL ZPTREC6(IFLTAB, ILBUFF, NTOT, IDADD, .FALSE.)
             IF (MLEVEL.GE.3) WRITE (MUNIT,190) CPATH(1:NPATH)
 190  FORMAT (' -----DSS--- ZCKPDD;  Updated Endian for Record: ',A)
         ELSE
            IF (MLEVEL.GE.3) WRITE (MUNIT,191) CPATH(1:NPATH)
 191  FORMAT (' -----DSS--- ZCKPDD;  Found Endian Issue for Record: ',A)
         ENDIF
      ENDIF
C
      ENDIF
      ENDIF
      ENDIF
      ENDIF
C
      IF (IFLTAB(KSTAT).NE.0) GO TO 950
C
      ENDIF
C
 200  CONTINUE
C
C
 700  CONTINUE
      IF (INSTR.GE.3) THEN
      IF (IFLTAB(KVERNO).LT.7900) THEN
C        Update the file version number to show that we have updated the
C        file
         CALL HOLCHR (IFLTAB(KVERS), 1, NVERSC, CTEMP, 1)
         CTEMP(3:4) = 'OA'
         CALL CHRHOL (CTEMP, 1, NVERSC, IFLTAB(KVERS), 1)
         NADD = 1
         IF(MLEVEL.GE.14)WRITE(MUNIT,*)'-ZNWRIT:  Update root'
         CALL ZPTREC6(IFLTAB, IFLTAB(KPERM), NPERM, NADD, .TRUE.)
         ISTAT = 2
      ENDIF
      ENDIF
C
      IF (NFIXED.GT.0) ISTAT = 3
C
 800  CONTINUE
      IF (INSTR.GE.3) THEN
      CALL ZMULTU6(IFLTAB, .FALSE., .TRUE.)
      IF (MLEVEL.GE.10) WRITE (MUNIT,820) ISTAT, NFIXED
 820  FORMAT (T6,'-----DSS---Debug:  Exit ZCKPDD, Status:', I3,
     *        '  Number repaired:',I6)
      ENDIF
!CALL FLUSH(MUNIT)                                                 Mu
C
      RETURN
C
 920  CONTINUE
      WRITE (MUNIT, 921)
 921  FORMAT (/' -----DSS---ZCKPDD:  ERROR;  File has Read Access Only')
      RETURN
C
C
C
 940  CONTINUE
      WRITE (MUNIT, 941) IFLTAB(KSTAT)
 941  FORMAT (/,' *****DSS*** ZCKPDD:  ERROR IN ACCESSING FILE ',
     * /,' Status: ',2I8)
      RETURN
C
 950  CONTINUE
      ISTAT = IFLTAB(KSTAT)
      IF (MLEVEL.GE.1) WRITE (MUNIT, 951) ISTAT, CPATH(1:NPATH)
 951  FORMAT (' -----DSS---ZWRITE:  ERROR;  Unable to store data,',
     * '  status:',I5,/,' Pathname: ',A)
      GO TO 800
C
      END

