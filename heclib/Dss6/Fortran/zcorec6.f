      SUBROUTINE zcorec6 (IFTOLD, IFTNEW, CPOLD, CPNEW,
     * IBUFF1, KBUFF1, IBUFF2, KBUFF2, LDUP, ISTAT)
C
C
C     Copy a single record using buffered reads and writes.
C     This allows us to copy as big as record as in the file
C     (unlimited size).
C     The record can be copied from another file, or can be duplicated
C     in the same file (with a different pathname)
C
C     Written by Bill Charley, HEC, 1989.
C
C
      INTEGER IFTOLD(*), IFTNEW(*), IBUFF1(*), IBUFF2(*)
      INTEGER KBUFF1, KBUFF2, ISTAT
      LOGICAL LDUP
      CHARACTER CPOLD*(*), CPNEW*(*)
C
      CHARACTER CDAT*9, CTIM*4, CUNITS*8, CTYPE*8
      CHARACTER CSDATE*9, CSTIME*4, CSPROG*8
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      LOGICAL LFOUND, LRETAG, L, LTSCOPY
      INTEGER INFORN(20)
      INTEGER JTYPE
C
      COMMON /WORDS/ IWORD(10)
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
C
C
      IF (MLEVEL.GE.11) THEN
          IF (LDUP) THEN
              WRITE (MUNIT, 20) IFTOLD(KUNIT), CPOLD,
     *        CPNEW, KBUFF1, KBUFF2
 20    FORMAT (T6,'-----DSS---Debug: Enter zcorec6, duplicate',/,
     * T5,'From:  Unit:',I5,'  Path: ',A,/,
     * T5,'  To: Path: ',A,/,
     * T5,'KBUFF1:',I8,',  KBUFF2:',I8)
          ELSE
          WRITE (MUNIT, 21) IFTOLD(KUNIT), CPOLD,
     * IFTNEW(KUNIT), CPNEW, KBUFF1, KBUFF2
 21    FORMAT (T6,'-----DSS---Debug: Enter zcorec6',/,
     * T5,'From:  Unit:',I5,'  Path: ',A,/,
     * T5,'  To:  Unit:',I5,'  Path: ',A,/,
     * T5,'KBUFF1:',I8,',  KBUFF2:',I8)
          ENDIF
      ENDIF
C
C
      IF (.NOT. LDUP) THEN
      IF (IFTOLD(KSWAP).NE.IFTNEW(KSWAP)) GO TO 950
      ENDIF
C
      ISTAT = 0
      IF ((KBUFF1.LE.0).OR.(KBUFF2.LE.0)) GO TO 900
C
C
C     Are we in a read only state?
      IF (LDUP) THEN
          IF (IFTOLD(KREADO).EQ.1) GO TO 940
          CALL zmultu6 ( IFTOLD, .TRUE., .TRUE.)
      ELSE
          IF (IFTNEW(KREADO).EQ.1) GO TO 940
C     Get multiple user access
          CALL zmultu6 ( IFTNEW, .TRUE., .TRUE.)          
      ENDIF
C

C
      CALL zreadx6 (IFTOLD, CPOLD, IBUFF1, KBUFF1, NIHEAD,
     * IBUFF2, KBUFF2, NCHEAD, IUHEAD, 0, NUHEAD, IDATA,
     * 0, NDATA, 2, LFOUND)
      IF (IFTOLD(KSTAT).NE.0) GO TO 960
      IF (.NOT.LFOUND) THEN
      ISTAT = 1
      GO TO 800
      ENDIF
      IF ((NIHEAD.GT.KBUFF1).OR.(NCHEAD.GT.KBUFF2)) GO TO 910
C
C     Save pertainent info from the info block
      NLEN = KIQUAL - KILNDA + 1
      DO 40 I=1,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFORN(I) = INFO(J)
 40   CONTINUE
      DO 45 I=1,NPASS
      J = I + NPPWRD + KIPASS - 1
      IPASS(I) = INFO(J)
 45   CONTINUE
C
      IDADD = INFO(NPPWRD+KIADAT)
      IHADD = INFO(NPPWRD+KIAUHE)
      ITYPE = INFO(NPPWRD+KITYPE)
      JTYPE = ITYPE
      NLDATA = INFO(NPPWRD+KILNDA)
      IPREC = INFO(NPPWRD+KIPREC)
      IF ((ITYPE.GE.110).AND.(ITYPE.LT.120)) THEN
C       Irregular interval - check for pseudo regular interval
        INTL_PSEUDO = IBUFF1(5)
      ENDIF
      CTAG = ' '
      CALL HOLCHR (INFO(NPPWRD+KITAG), 1, NTAGC, CTAG, 1)
C
      CALL CHRLNB(CPNEW, N)
      IF (MLEVEL.GE.3) WRITE (MUNIT,50) CPNEW(1:N)
 50   FORMAT(' -----DSS--- zcorec6;  Record: ',A)
C
C
C     If this data is time-series, and we need to use the compression
C     method of the new file, call zrrtsx6, then zsrtsx6 to do this,
C     or it we are copying 15 or 30 min data.
      LTSCOPY = .FALSE.
      IF (JTYPE.EQ.100) THEN
      IF ((LTSCMP).AND.(INFO(NPPWRD+KIQUAL).EQ.0)) THEN
          IF (.NOT. LDUP) THEN
          IF  ((IFTNEW(KCOMPN).GT.0).OR.(INFO(NPPWRD+KICOMP).GT.0)) THEN
          ENDIF
      LTSCOPY = .TRUE.
      ENDIF
      ENDIF
      ENDIF
C
      IF (LTSCOPY) THEN
      CDAT = ' '
      NVALS = KBUFF1
      CALL zrrtsx (IFTOLD, CPOLD, CDAT, CTIM, NVALS, IBUFF1, IDUM,
     * .FALSE., L, CUNITS, CTYPE, IBUFF2, KBUFF2, NUHEAD, IOFSET,
     * J, JSTAT)
      IF (JSTAT.GE.5) GO TO 920
C
C     Reset the record information to match the old record
      ISVER = IBVER
      ISPREC = IPREC
      CSPROG = CPROG(1:8)
      CSDATE = CDATE(1:9)
      CSTIME = CTIME(1:4)
      IBVER = INFO(NPPWRD+KIVER)
      IPREC = INFO(NPPWRD+KIPREC)
      CALL HOLCHR (INFO(NPPWRD+KIPROG), 1, NPROGC, CPROG, 1)
      CALL HOLCHR (INFO(NPPWRD+KIDATE), 1, NDATEC, CDATE, 1)
      CALL HOLCHR (INFO(NPPWRD+KITIME), 1, NTIMEC, CTIME, 1)
C
C     Get the date and time of the data (and adjust the offset)
      CALL zupath (CPOLD, IBPART, IEPART, ILPART, I)
      CDAT = CPOLD(IBPART(4):IEPART(4))
      CALL DATJUL (CDAT, JUL, IERR)
      I = 1
      CALL zgintl6 (INTL, CPOLD(IBPART(5):IEPART(5)), N, I)
      ITIME = 1
      CALL zofset6 (JUL, ITIME, INTL, 2, IOFSET)
      CALL JULDAT (JUL, 104, CDAT, N)
      N = M2IHM (ITIME, CTIM)
C
      IF (LDUP) THEN
      CALL zsrtsx (IFTOLD, CPNEW, CDAT, CTIM, NVALS, IBUFF1, IDUM,
     * .FALSE., CUNITS, CTYPE, IBUFF2, NUHEAD, 0, 0, B, L, L, N, JSTAT)          
          ELSE
      CALL zsrtsx (IFTNEW, CPNEW, CDAT, CTIM, NVALS, IBUFF1, IDUM,
     * .FALSE., CUNITS, CTYPE, IBUFF2, NUHEAD, 0, 0, B, L, L, N, JSTAT)
       ENDIF
C
C     Reset the record information
      IBVER = ISVER
      IPREC = ISPREC
      CPROG = CSPROG
      CDATE = CSDATE
      CTIME = CSTIME
C
      GO TO 800
      ENDIF
C
C
C     Check if new record exists
      LWRITE = .TRUE.
      CALL CHRLNB (CPNEW, NPNEW)
      
      IF (LDUP) THEN
           CALL zcheck6 (IFTOLD, CPNEW, NPNEW, JHEAD, JDATA, LFOUND)
C     If the pathname was not found by zcheck6 write new pointers
C
      IF (.NOT.LFOUND) THEN
      CALL znwrit6 (IFTOLD, CPNEW, NPNEW, NIHEAD, NCHEAD, NUHEAD, NDATA)
      ELSE
      IF (LPROTC) GO TO 930
      CALL zowrit6 (IFTOLD, CPNEW, NPNEW, NIHEAD, NCHEAD, NUHEAD,
     * NDATA)
      ENDIF
      IF (IFTOLD(KSTAT).NE.0) GO TO 960
      
      ELSE
          
      CALL zcheck6 (IFTNEW, CPNEW, NPNEW, JHEAD, JDATA, LFOUND)
C     If the pathname was not found by zcheck6 write new pointers
C
      IF (.NOT.LFOUND) THEN
      CALL znwrit6 (IFTNEW, CPNEW, NPNEW, NIHEAD, NCHEAD, NUHEAD, NDATA)
      ELSE
      IF (LPROTC) GO TO 930
      CALL zowrit6 (IFTNEW, CPNEW, NPNEW, NIHEAD, NCHEAD, NUHEAD,
     * NDATA)
      ENDIF
      IF (IFTNEW(KSTAT).NE.0) GO TO 960
      ENDIF
C
C     Update the information block to contain what the old one had
C
      DO 60 I=1,NLEN
      J = I + NPPWRD + KILNDA - 1
      INFO(J) = INFORN(I)
 60   CONTINUE
      DO 70 I=1,NPASS
      J = I + NPPWRD + KIPASS - 1
      INFO(J) = IPASS(I)
 70   CONTINUE
C
      ISIZE = NPPWRD + NINFO
      IF (LDUP) THEN
      CALL zptrec6 (IFTOLD, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
C     Now store the internal header and the compression headers
C     Store the header array
      IF (NIHEAD.GT.0)
     *CALL zptrec6(IFTOLD, IBUFF1, NIHEAD, INFO(NPPWRD+KIAIHE), .FALSE.)
      IF (NCHEAD.GT.0)
     *CALL zptrec6(IFTOLD, IBUFF2, NCHEAD, INFO(NPPWRD+KIACHE), .FALSE.)
          ELSE
      CALL zptrec6 (IFTNEW, INFO, ISIZE, IPNBIN(JPNBIN+NPPWRD+KBAINF),
     * .FALSE.)
C     Now store the internal header and the compression headers
C     Store the header array
      IF (NIHEAD.GT.0)
     *CALL zptrec6(IFTNEW, IBUFF1, NIHEAD, INFO(NPPWRD+KIAIHE), .FALSE.)
      IF (NCHEAD.GT.0)
     *CALL zptrec6(IFTNEW, IBUFF2, NCHEAD, INFO(NPPWRD+KIACHE), .FALSE.)
      ENDIF
C
C
C     Copy the Users Header block
      IF (NUHEAD.GT.0) THEN
      NH = MIN0(NUHEAD,KBUFF1)
      NHTOT = 0
      JHADD = INFO(NPPWRD+KIAUHE)
 100  CONTINUE
      CALL zgtrec6 (IFTOLD, IBUFF1, NH, IHADD, .FALSE.)
      IF (LDUP) THEN
      CALL zptrec6 (IFTOLD, IBUFF1, NH, JHADD, .FALSE.)
      ELSE
      CALL zptrec6 (IFTNEW, IBUFF1, NH, JHADD, .FALSE.)
      ENDIF
      NHTOT = NHTOT + NH
      IF (NHTOT.LT.NUHEAD) THEN
      IHADD = IHADD + NH
      JHADD = JHADD + NH
      NH = NUHEAD - NHTOT
      NH = MIN0(NH,KBUFF1)
      GO TO 100
      ENDIF
      ENDIF
C
C     Copy the data array
      IF (NDATA.GT.0) THEN
      ND = MIN0(NDATA,KBUFF1)
      NDTOT = 0
      JDADD = INFO(NPPWRD+KIADAT)
 120  CONTINUE
      CALL zgtrec6 (IFTOLD, IBUFF1, ND, IDADD, .FALSE.)
C     Do we need to swap words on version conversion doubles?
      IF (.NOT. LDUP) THEN
      IF (IFTOLD(KDSWAP).NE.IFTNEW(KDSWAP)) THEN
         IF (JTYPE.EQ.205) THEN
            CALL zdswap6(IBUFF1, ND)
            IF (MLEVEL.GE.11) WRITE (MUNIT, *)'Swapping PDD'
         ELSE IF (JTYPE.EQ.105) THEN
            IF (MLEVEL.GE.11) WRITE (MUNIT, *)'Swapping RTD'
            IF (INFO(NPPWRD+KIQUAL).EQ.0) THEN
               CALL zdswap6(IBUFF1, ND)
            ELSE
               N = (ND * 2) / 3
               CALL zdswap6(IBUFF1, N)
            ENDIF
         ELSE IF (JTYPE.EQ.115) THEN
            IF (MLEVEL.GE.11) WRITE (MUNIT, *)'Swapping ITD'
            IMULT = 3
            IF (INFO(NPPWRD+KIQUAL).NE.0) IMULT = 4
            DO 140, I=1,ND,IMULT
               CALL zdswap6(IBUFF1(I+1), 1)
 140        CONTINUE
         ENDIF
      ENDIF
      CALL zptrec6 (IFTNEW, IBUFF1, ND, JDADD, .FALSE.)
      ELSE
      CALL zptrec6 (IFTOLD, IBUFF1, ND, JDADD, .FALSE.)
      ENDIF
      NDTOT = NDTOT + ND
      IF (NDTOT.LT.NDATA) THEN
      IDADD = IDADD + ND
      JDADD = JDADD + ND
      ND = NDATA - NDTOT
      ND = MIN0(ND,KBUFF1)
      GO TO 120
      ENDIF
      ENDIF
      IF (.NOT. LDUP) THEN
      IF (IFTNEW(KSTAT).NE.0) GO TO 960
      ENDIF
      IF (IFTOLD(KSTAT).NE.0) GO TO 960
C
C
C
 800  CONTINUE
C     Release multiple user access
      IF (LDUP) THEN
      CALL zmultu6 ( IFTOLD, .FALSE., .TRUE.)
      ELSE
      CALL zmultu6 ( IFTNEW, .FALSE., .TRUE.)
      ENDIF
      LWRITE = .FALSE.
      INTL_PSEUDO = 0
      IF (MLEVEL.GE.11) WRITE (MUNIT, 820)
 820  FORMAT (T6,'-----DSS---Debug: Exit zcorec6')
*      CALL FLUSH(MUNIT)                                        
      RETURN
C
 900  CONTINUE
      ISTAT = -1
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901) KBUFF1, KBUFF2
 901  FORMAT (/,' -----DSS---zcorec6:  ERROR;  Null length buffer(s)',
     * ' supplied for copy',/,' Buffer Sizes:',2I8,/)
      GO TO 800
C
 910  CONTINUE
      ISTAT = -2
      IF (MLEVEL.GE.2) WRITE (MUNIT, 911) NIHEAD, KBUFF1, NCHEAD, KBUFF2
 911  FORMAT (/,' -----DSS---zcorec6:  ERROR;  Insufficient buffer(s)',
     * ' supplied for copy',/,' Needed:',I9,',  Supplied:',I9,
     * ';    Needed:',I9,',  Supplied:',I9)
      GO TO 800
C
 920  CONTINUE
      ISTAT = 1
      IF (MLEVEL.GE.2) WRITE (MUNIT, 921) CPOLD, JSTAT
 921  FORMAT (/,' -----DSS---zcorec6:  ERROR;  Unable to Retrieve Data',
     * /,' Pathname: ',A,/,' Status:',I5)
      GO TO 800
C
 930  CONTINUE
      ISTAT = 2
      IF (MLEVEL.GE.2) WRITE (MUNIT, 931) CPNEW
 931  FORMAT (' -----DSS---zcorec6:  Write Protection for Existing',
     * ' Record (no data written)',/,
     * ' Pathname: ',A)
C
C
 940  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 941) CPNEW
 941  FORMAT (' -----DSS---zcorec6:  ERROR;  File has Read Access Only',
     * /,' Pathname: ',A)
      ISTAT = -12
      GO TO 800
C
 950  CONTINUE
      ISTAT = -5
      WRITE (MUNIT, 951)
 951  FORMAT (/' -----DSS---zcorec6:  ERROR;  Files are of different',
     * ' endian type.'/' Can only copy files with the same byte order')
      GO TO 800
C
C
 960  CONTINUE
      IF (LDUP) THEN
      ISTAT = IFTOLD(KSTAT)
      WRITE (MUNIT, 961) IFTOLD(KSTAT)
      ELSE
      ISTAT = IFTNEW(KSTAT)
      WRITE (MUNIT, 961) IFTNEW(KSTAT)
      ENDIF
 961  FORMAT (/,' *****DSS*** zcorec6:  ERROR  - UNABLE TO ',
     * ' COPY DATA',/,' Status: ',I8)
      GO TO 800
C
      END
      SUBROUTINE zcorec(ifltabFrom, ifltabTo, cpathFrom, cpathTo,
     * IBUFF1, KBUFF1, IBUFF2, KBUFF2, LDUP, ISTAT)
C
      INTEGER ifltabFrom(*)
      CHARACTER cpathFrom*(*)
      INTEGER ifltabTo(*)
      CHARACTER cpathTo*(*)
C
      INTEGER IBUFF1(*), IBUFF2(*)
      LOGICAL LDUP
      INTEGER KBUFF1, KBUFF2, ISTAT
C
      call zGetVersion(ifltabFrom, iversion1)
      IF (LDUP) THEN
      iversion2 =  iversion1    
      ELSE
      call zGetVersion(ifltabTo, iversion2)
      ENDIF
C
      if (iversion1.eq.iversion2) then
        IF (iversion1.EQ.6) THEN
           call zcorec6(ifltabFrom, ifltabTo, cpathFrom, cpathTo,
     *                  IBUFF1, KBUFF1, IBUFF2, KBUFF2, LDUP, ISTAT)
        else
            if (cpathFrom.eq.cpathTo) then
C  *******   FIX ME ****************
            call zcopyRecord (ifltabFrom, ifltabTo, cpathFrom, cpathTo,
     *                        ISTAT)
            else
            call zcopyRecord (ifltabFrom, ifltabTo, cpathFrom, cpathTo,
     *                        ISTAT)
            endif
        endif
      else
        call zcopyRecord (ifltabFrom, ifltabTo, cpathFrom, cpathTo,
     * ISTAT)
      endif
      IF (iversion.EQ.6) THEN
      call zstags6 (IFLTAB, CLINE, ISTAT)
      endif
      return
      end

