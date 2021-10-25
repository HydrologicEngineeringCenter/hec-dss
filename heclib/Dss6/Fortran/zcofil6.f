      SUBROUTINE zcofil6 (IFTOLD, IFTNEW, IARRAY, KARRAY, IBUFF, KBUFF,
     * LRCDEL, LRETAGX)
C
C
C     Z-Copy-File.
C     Copies all records in file opened with IFTNEW into
C     file opened with IFTOLD.
C     Records that have been deleted (but have not yet been
C     squeezed out) may be restored by setting LRCDEL to true.
C     New tags will be generated if LRETAG is set to true.
C
C     zcofil6 copies via a brute-force approach;  Thus it
C     will copy all vaild data from a damaged file
C     (restoring it as best as possible).
C
C     Written by Bill Charley at HEC, 1988.
C
      INTEGER IFTNEW(*), IFTOLD(*), IBUFF(KBUFF), INFOCO(30)
      INTEGER IARRAY(KARRAY)
      LOGICAL LRCDEL, LRETAG, LFOUND, LPROT2, LTSCOPY, LRETAGX, LDUP
      CHARACTER CPATH*400
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
      COMMON /ZSTATUS/ TOTAL_NUMB,  CURRENT_NUMB,
     *                 INTERRUPT, NERROR, MAXERROR
      INTEGER TOTAL_NUMB,  CURRENT_NUMB,  INTERRUPT
C
C
C
C     The follow is if another thread interrupts us to stop the process
      INTERRUPT = 0
      LRETAG = .FALSE.
      IF (MLEVEL.GE.11) WRITE (MUNIT,20) IFTOLD(KUNIT)
 20   FORMAT (T6,'-----DSS---Debug:  Enter zcofil6;  Unit:',I5)
C
C     Make Sure that we have enough space to work with
      IF ((KARRAY.LT.NBSIZE).OR.(KBUFF.LT.NBSIZE)) GO TO 900
C
C     Be sure the files are same type endian!
      IF (IFTOLD(KSWAP).NE.IFTNEW(KSWAP)) GO TO 930
C
C     Make sure that we are copying the same version
      IF ((IFTOLD(1).NE.6).OR.(IFTNEW(1).NE.6)) GO TO 910
      LPROT2 = LPROTC
      LCOFIL = .TRUE.
C
C     Are we in a read only state?
      IF (IFTNEW(KREADO).EQ.1) GO TO 920
C
C     Print any squeeze status message
C     IF (LSQSTA) CALL CHRWT (MUNIT, CHAR(13)//CHAR(10)//'  0% Complete'
C    * // CHAR(13), 16)
C     IF (LSQSTA) CALL CHRWT (MUNIT, ' 0% Complete' // CHAR(13), 14)    u
      !IF (LSQSTA) CALL CHRWT (MUNIT, '+ 0% Complete' // CHAR(13), 14)   Md
C
C     Get the number of physical records in file (not data recs)
      IADD = IFTOLD(KFSIZE) - 1
      CALL zgetrw6 (IADD, NRECS, IW)
C
      TOTAL_NUMB = IFTOLD(KNRECS)
      IFTNEW(KCOLL) = IFTOLD(KCOLL)
      CURRENT_NUMB = 0
C
      DO 200 IREC=1,NRECS
C
C     If another thead set the interrupt flag, return
      IF (INTERRUPT.NE.0) GO TO 840
C
C     Should the status (% complete) be printed
      IF (LSQSTA) THEN
      N = MOD (IREC,10)
      IF (N.EQ.0) THEN
      IP = ((IREC-1)*100) / NRECS
      WRITE (CPATH, 25) IP
C25   FORMAT (I3,'% Complete')                                          u
 25   FORMAT ('+',I3,'% Complete')                                      Md
      !CALL CHRWT (MUNIT, CPATH(1:14)//CHAR(13), 15)
      ENDIF
      ENDIF
C
C     Read physical record IREC from file
      CALL zgetad6 (IADD, IREC, 1)
      CALL zgtrec6 (IFTOLD, IARRAY, NBSIZE, IADD, .FALSE.)
C
C     Did we hit end of file?
      IF (IADD.LT.0) THEN
      WRITE (MUNIT, 28) IREC
 28   FORMAT (//, ' **** Caution:  Reached End of File at Physical',
     * ' Record:',I8,/,' **** Probable Incomplete File Copy',/)
      GO TO 800
      ENDIF
C
C     Search through this record, looking for pathname flags
C
      DO 199 IWRD=1,NBSIZE
C
      IF (IARRAY(IWRD).EQ.NPFLAG) THEN
C
C     Found a flag - Get the first three words of the information
C     block to see if this indeed is the start of a data record
      CALL zgetad6 (IADD, IREC, IWRD)
      CALL zgtrec6 ( IFTOLD, INFO, 3, IADD, .FALSE.)
      IF (IFTOLD(KSTAT).NE.0) GO TO 940
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,30)IADD, INFO(KISTAT), INFO(KINPAT)
 30   FORMAT (T5,'Found a INFO Flag at address',I13,/,
     * T5,'Status:',I5,';  Pathname Length:',I5)
      IPSTAT = INFO(KISTAT)
      IF (IPSTAT.GT.10) IPSTAT = IPSTAT - 10
C     Is this an undelete of a renamed or deleted record?
      IF ((IPSTAT.EQ.2).AND.(LRCDEL)) INFO(KISTAT) = INFO(KISTAT) - 1
C     Check for a valid status flag (1)
      IF (IPSTAT.EQ.1) THEN
C     Check for a valid pathname length
      NPATH = INFO(KINPAT)
      IF ((NPATH.GT.0).AND.(NPATH.LE.392)) THEN
C
C     Passed, therefore a valid record - Get the full information block
      NPPWRD = (NPATH - 1)/NCPW + 1
      NPMWRD = (NPATH - 1)/NCMW + 1
      CALL zgtrec6 ( IFTOLD, INFO, NINFO+NPPWRD, IADD, .FALSE.)
      IF (IFTOLD(KSTAT).NE.0) GO TO 940
C
C     Get the pathname
      CPATH = ' '
      CALL HOLCHR (INFO(KIPATH), 1, NPATH, CPATH, 1)
      CURRENT_NUMB = CURRENT_NUMB + 1
C
C
C     If this data is time-series, and we need to use the compression
C     method of the new file, or we are copying 15 or 30 min data,
C     call zrrtsx6, then zsrtsx6 via zcorec6
      LTSCOPY = .FALSE.
      IF (INFO(NPPWRD+KITYPE).EQ.100) THEN
      IF ((IFTOLD(KVERNO).LT.67300).OR.(IFTNEW(KVERNO).LT.67300)) THEN
      IF (INDEX(CPATH,'15MIN').GT.0) LTSCOPY = .TRUE.
      IF (INDEX(CPATH,'20MIN').GT.0) LTSCOPY = .TRUE.
      IF (INDEX(CPATH,'30MIN').GT.0) LTSCOPY = .TRUE.
      ENDIF
      IF ((LTSCMP).AND.(INFO(NPPWRD+KIQUAL).EQ.0)) THEN
      IF  ((IFTNEW(KCOMPN).GT.0).OR.(INFO(NPPWRD+KICOMP).GT.0)) THEN
      LTSCOPY = .TRUE.
      ENDIF
      ENDIF
      ENDIF
C
C     If converting from a previous version with a possible double
C     swap miss-match, use zcorec6 also
      IF (IFTOLD(KDSWAP).NE.IFTNEW(KDSWAP)) LTSCOPY = .TRUE.
C
      IF (LTSCOPY) THEN
      IF (LRETAG) THEN
      IST = -5
      ELSE
      IST = 0
      ENDIF
      LDUP = .FALSE.
      CALL zcorec6 (IFTOLD, IFTNEW, CPATH(1:NPATH), CPATH(1:NPATH),
     * IARRAY, KARRAY, IBUFF, KBUFF, LDUP, IST)
      IF (IFTNEW(KSTAT).NE.0) GO TO 940
      LPROTC = LPROT2
C     Re-read the record that we just destroyed
      CALL zgetad6 (IADD, IREC, 1)
      CALL zgtrec6 (IFTOLD, IARRAY, NBSIZE, IADD, .FALSE.)
      GO TO 200
C
      ENDIF
C
C
C     Save pertainent info from the info block
      NIHEAD = INFO(NPPWRD+KINIHE)
      NCHEAD = INFO(NPPWRD+KINCHE)
      NUHEAD = INFO(NPPWRD+KINUHE)
      NDATA  = INFO(NPPWRD+KINDAT)
      IIHADD = INFO(NPPWRD+KIAIHE)
      ICHADD = INFO(NPPWRD+KIACHE)
      IUHADD = INFO(NPPWRD+KIAUHE)
      IDADD  = INFO(NPPWRD+KIADAT)
      ITYPE  = INFO(NPPWRD+KITYPE)
      NLDATA = INFO(NPPWRD+KILNDA)
      IF (LRETAG) THEN
      CTAG = ' '
      ELSE
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CTAG, 1)
      ENDIF
C
      NLEN = KIQUAL - KILNDA + 1
      DO 40 I=1,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFOCO(I) = INFO(J)
 40   CONTINUE
      DO 45 I=1,NPASS
      J = I + NPPWRD + KIPASS - 1
      IPASS(I) = INFO(J)
 45   CONTINUE
C
C
C     Check for pseudo regular time series data
      IF ((ITYPE.GE.110).AND.(ITYPE.LE.120)) THEN
        IADD = IIHADD + 4
        CALL ZGTREC6 (IFTOLD, INTL_PSEUDO, 1, IADD, .FALSE.)
      ENDIF
C
C
C     Check for addresses out of range.
      IF ((IDADD.LT.100).OR.(IDADD.GE.IFTOLD(KFSIZE))) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'Data address',
     *                                    IADD, IDADD, CPATH(1:NPATH)
 50   FORMAT(/,' -----DSS*** zcofil6;  Error: ',A,' out of Range',
     * /,' Unable to Copy Record;',/,' Info Block Address:',I12,
     * ',   Value:',I12,/,' Pathname: ',A)
      GO TO 200
      ENDIF
      IF ((NIHEAD.GT.0).AND.
     *   ((IIHADD.GE.IFTOLD(KFSIZE)).OR.(IIHADD.LE.0))) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'Internal header address',
     *                                    IADD, IIHADD, CPATH(1:NPATH)
      GO TO 200
      ENDIF
      IF ((NCHEAD.GT.0).AND.
     *   ((ICHADD.GE.IFTOLD(KFSIZE)).OR.(ICHADD.LE.0))) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'Compression header address',
     *                                    IADD, ICHADD, CPATH(1:NPATH)
      GO TO 200
      ENDIF
      IF ((NUHEAD.GT.0).AND.
     *   ((IUHADD.GE.IFTOLD(KFSIZE)).OR.(IUHADD.LE.0))) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'User header address',
     *                                    IADD, IUHADD, CPATH(1:NPATH)
      GO TO 200
      ENDIF
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
C     Do a sanity check on our numbers
      IF ((NIHEAD.GT.100000).OR.(NIHEAD.LT.0)) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'Internal Header values',
     *                                    IADD, NIHEAD, CPATH(1:NPATH)
      GO TO 200
      ENDIF
      IF ((NCHEAD.GT.100000).OR.(NCHEAD.LT.0)) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'Compression Header values',
     *                                    IADD, NCHEAD, CPATH(1:NPATH)
      GO TO 200
      ENDIF
      IF ((NUHEAD.GT.100000).OR.(NUHEAD.LT.0)) THEN
      IF (MLEVEL.GE.0) WRITE (MUNIT, 50) 'User Header values',
     *                                    IADD, NUHEAD, CPATH(1:NPATH)
      GO TO 200
      ENDIF
C
C     Write the data record to the DSS file w/ IFTNEW
C
C     Get multiple user access
      LWRITE = .FALSE.
      CALL zmultu6 ( IFTNEW, .TRUE., .TRUE.)
C
C     Check if new record exists
      CALL zcheck6 (IFTNEW, CPATH(1:NPATH), NPATH, JHEAD, JDATA, LFOUND)
      IF (IFTNEW(KSTAT).NE.0) GO TO 940
C
C
C     If the pathname was not found by zcheck6 write new pointers
C
      IF (.NOT.LFOUND) THEN
      CALL znwrit6 (IFTNEW, CPATH(1:NPATH), NPATH, NIHEAD, NCHEAD,
     * NUHEAD, NDATA)
      ELSE
      IF (LPROT2) THEN
      IF (MLEVEL.GE.2) WRITE (MUNIT, 60) CPATH(1:NPATH)
 60   FORMAT (' -----DSS---zcofil6:  Write Protection for Existing',
     * ' Record (no data written)',/,
     * ' Pathname: ',A)
      CALL zmultu6 ( IFTNEW, .FALSE., .TRUE.)
      GO TO 200
      ENDIF
      CALL zowrit6 (IFTNEW, CPATH(1:NPATH), NPATH, NIHEAD, NCHEAD,
     * NUHEAD, NDATA)
      ENDIF
C
C
C     Update the information block to contain what the old one had
C     But don't copy the tag
C
      NLEN = KITIME - KILNDA + 1
      DO 80 I=1,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFO(J) = INFOCO(I)
 80   CONTINUE
C
      NLEN = KIQUAL - KILNDA + 1
      IBEG = KITYPE - KILNDA + 1
      DO 90 I=IBEG,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFO(J) = INFOCO(I)
 90   CONTINUE
      DO 100 I=1,NPASS
      J = I + NPPWRD + KIPASS - 1
      INFO(J) = IPASS(I)
 100  CONTINUE
C
      ISIZE = NPPWRD + NINFO
      CALL zptrec6 (IFTNEW, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
      IF (IFTNEW(KSTAT).NE.0) GO TO 940
C
C     Now copy the headers and the data array
C
C     Internal header
      IF (NIHEAD.GT.0) THEN
      N = MIN0(NIHEAD,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIAIHE)
 120  CONTINUE
      CALL zgtrec6 (IFTOLD, IBUFF, N, IIHADD, .FALSE.)
      CALL zptrec6 (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NIHEAD) THEN
      IIHADD = IIHADD + N
      IADD = IADD + N
      N = NIHEAD - NTOT
      N = MIN0(N,KBUFF)
      GO TO 120
      ENDIF
      ENDIF
C
C     Compression header
      IF (NCHEAD.GT.0) THEN
      N = MIN0(NCHEAD,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIACHE)
 140  CONTINUE
      CALL zgtrec6 (IFTOLD, IBUFF, N, ICHADD, .FALSE.)
      CALL zptrec6 (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NCHEAD) THEN
      ICHADD = ICHADD + N
      IADD = IADD + N
      N = NCHEAD - NTOT
      N = MIN0(N,KBUFF)
      GO TO 140
      ENDIF
      ENDIF
C
C     User Header
      IF (NUHEAD.GT.0) THEN
      N = MIN0(NUHEAD,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIAUHE)
 160  CONTINUE
      CALL zgtrec6 (IFTOLD, IBUFF, N, IUHADD, .FALSE.)
      CALL zptrec6 (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NUHEAD) THEN
      IUHADD = IUHADD + N
      IADD = IADD + N
      N = NUHEAD - NTOT
      N = MIN0(N,KBUFF)
      GO TO 160
      ENDIF
      ENDIF
C
C     Data Array
      IF (NDATA.GT.0) THEN
      N = MIN0(NDATA,KBUFF)
      NTOT = 0
      IADD = INFO(NPPWRD+KIADAT)
 180  CONTINUE
      CALL zgtrec6 (IFTOLD, IBUFF, N, IDADD, .FALSE.)
      CALL zptrec6 (IFTNEW, IBUFF, N, IADD,   .FALSE.)
      NTOT = NTOT + N
      IF (NTOT.LT.NDATA) THEN
      IDADD = IDADD + N
      IADD = IADD + N
      N = NDATA - NTOT
      N = MIN0(N,KBUFF)
      GO TO 180
      ENDIF
      ENDIF
C
      IF (MLEVEL.GE.3) WRITE (MUNIT,190) CPATH(1:NPATH)
 190  FORMAT (' -----DSS--- zcofil6;  Record: ',A)
C
C     Release multiple user access
      CALL zmultu6 ( IFTNEW, .FALSE., .TRUE.)
      LWRITE = .FALSE.
C
      ENDIF
      ENDIF
      ENDIF
C
      IF (IFTOLD(KSTAT).NE.0) GO TO 940
      IF (IFTNEW(KSTAT).NE.0) GO TO 940
C
 199  CONTINUE
 200  CONTINUE
C
C
C     IF (LSQSTA) CALL CHRWT (MUNIT, '100' // CHAR(13) // CHAR(10), 6)  u
      !IF (LSQSTA) CALL CHRWT (MUNIT, '+100' // CHAR(13) // CHAR(10), 6) Md
C
 800  CONTINUE
      LSQSTA = .FALSE.
      LTSCMP = .FALSE.
      LPROTC = .FALSE.
      LCOFIL = .FALSE.
      IF (MLEVEL.GE.11) WRITE (MUNIT,820)
 820  FORMAT (T6,'-----DSS---Debug:  Exit zcofil6')
      !CALL FLUSH(MUNIT)                                                 Mu
C
      RETURN
C
C
 840  CONTINUE
      IF (MLEVEL.GE.4) WRITE (MUNIT, 850)
 850  FORMAT (' ----- DSS --- zcofil6:  Copy interrupted.')
      GO TO 800
C
C
 900  CONTINUE
      WRITE (MUNIT, 901) KARRAY, KBUFF, NBSIZE
 901  FORMAT (/,' ----- DSS --- zcofil6:  ERROR;  Buffer size(s) not',
     * ' Large enough to copy file',/,' Sizes provided:',2I8,
     * ';   Size Required:',I5,/)
      CALL zabort6 (IFTOLD, 331, 'zcofil6 ', KBUFF, NBSIZE,
     * 'Buffer sizes not large enough')
      RETURN
C
 910  CONTINUE
      WRITE (MUNIT, 911) IFTOLD(1), IFTNEW(1)
 911  FORMAT (/,' ----- DSS --- zcofil6:  ERROR;  DSS File(s) not',
     * ' Version 6.',/,' Versions:',2I8,/)
      RETURN
C
C
 920  CONTINUE
      WRITE (MUNIT, 921)
 921  FORMAT(/' -----DSS---zcofil6: ERROR;  File has Read Access Only')
      RETURN
C
C
 930  CONTINUE
      WRITE (MUNIT, 931)
 931  FORMAT (/' -----DSS---zcofil6:  ERROR;  Files are of different',
     * ' endian type.'/' Can only copy files with the same byte order')
      RETURN
C
C
 940  CONTINUE
      WRITE (MUNIT, 941) IFTNEW(KSTAT), IFTOLD(KSTAT)
 941  FORMAT (/,' *****DSS*** zcofil6:  ERROR  - UNABLE TO ',
     * ' COPY DATA',/,' Status: ',2I8)
      RETURN
C
      END

