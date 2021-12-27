      SUBROUTINE zritsi6(IFLTAB, CPATHNAME, JULS, ISTIME, JULE, IETIME,
     * LGETDOB, LFILDOB, ITIMES, SVALUES, DVALUES, KVALS, NVALS,
     * IBDATE, IQUAL, LQUAL, LQREAD, CUNITS, CTYPE, IUHEAD, KUHEAD,
     * NUHEAD, COORDS, ICDESC, LCOORDS, INFLAG, ISTAT)
C
C
C     Main subroutine for retrieving irregular time series data
C
C     Written by Bill Charley
C
C        LGETDOB: Logical set to TRUE if the data should be returned
C                 as DOUBLES in array DVALUES (SVALUES ignored), or
C                 FALSE if the data is to be returned as REALS in
C                 array SVALUES (DVALUES ignored).
C
C     Output:
C        NVALS:   The number of data retrieved.  Note that this is
C                 also and input argument.
C        LFILDOB: Logical set to TRUE if the data on disk is stored
C                 as DOUBLES (FALSE if data is REAL)
C        SVALUES: REAL array to contain data if LGETDOB is FALSE.
C        DVALUES: DOUBLE array to contain data if LGETDOB is TRUE.
C        CUNITS:  Character string returning the units of the data.
C                 CUNITS must be declared CHARACTER*8
C        CTYPE:   Character string returning the type of the data
C                 (e.g., PER-AVER).  CTYPE must be declared CHARACTER*8
C
C     09JUL87 - Alaric Clinton  add the next and
C    previous (see ISTAT) capability
C
C     variables
C       LPREV - get the previous data value
C       LCASE - special case of the previous value being the last
C                value of the previous block
C       LNEXT - get the next data value
C       IFORWD - (1) increment time (-1) decrement time
C
C     special cases
C       1) previous value not in the current record
C       2) next value not in the current record
C       3) time window contains an initial empty intervals
C       4) time window contains internal empty intervals
C       5) entire time window is empty
C
C     notes
C       1) should the time window be empty, NVALS is returned as zero;
C          otherwise one or two extra data values may be inclued
C          depending on ISTAT
C
      INTEGER IFLTAB(*), IQUAL(*), ITIMES(*), IUHEAD(*), ICDESC(*)
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*), COORDS(*)
      LOGICAL LQUAL, LQREAD, LCOORDS
      LOGICAL LGETDOB, LFILDOB
C
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      PARAMETER (KIHEAD=28)
      INTEGER IIHEAD(KIHEAD)
      INTEGER NIHEAD, NNIHEAD
C
      CHARACTER CPATHNAME*(*), CUNITS*(*), CTYPE*(*)
      CHARACTER CPATH1*392, CRDATE*9, CDATE1*9, CDATE2*9, CSCRAT*20
      CHARACTER CPATH*392, CTEMP*4
C
      REAL SVAL(1)
      DOUBLE PRECISION DVAL(1)
      INTEGER IQ(1)
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'verticalDatumFortran.h'
C
      LOGICAL LF, LFOUND, LGETQ, LPREV, LNEXT, LCASE, LDSWAP
      INTEGER IFORWD, INTLPS
C
C     Pathname variable dimensions
      character*64 ca, cb, cc, cd, ce, cf
      integer na, nb, nc, nd, ne, nf, npath
C
C     Vertical datum varible dimensions
      character*400 vdiStr, errMsg
      character*16 nativeDatum, unit
      double precision offsetNavd88, offsetNgvd29, vertDatumOffset
      logical l_Navd88Estimated, l_Ngvd29Estimated
      integer vdiStrLen
C
C
C
C
C
      IKSTAT = ISTAT
      LFILDOB = .FALSE.
      IRTZONE = -1
      CRTZONE = ' '
      NIHEAD = 0
      NNIHEAD = 0
	LCOORDS = .FALSE.
      LDSWAP = .FALSE.
      CPATH = CPATHNAME
      CALL CHRLNB(CPATHNAME, NPATH)
      IF (IFLTAB(KDSWAP).NE.0) LDSWAP = .TRUE.
      CALL zset6('QUAL', 'OFF', 0)
C
      IF (MLEVEL.GE.9) THEN
      CDATE1 = ' '
      CDATE2 = ' '
      IF (ISTIME.GE.0) THEN
      CALL JULDAT (JULS, 114, CDATE1, NDATE1)
      CALL JULDAT (JULE, 114, CDATE2, NDATE2)
      ENDIF
      WRITE (MUNIT, 10) JULS, ISTIME, CDATE1, JULE, IETIME, CDATE2
 10   FORMAT (T10,'----- ENTERING zritsx6-----',
     * /,T5,'Starting date and time:',3X,2I8,2X,A,
     * /,T5,'Ending date and time:  ',3X,2I8,2X,A)
      WRITE (MUNIT, 20) CPATH(1:NPATH)
 20   FORMAT (T5,'Pathname: ',A)
      WRITE (MUNIT,30) KVALS, INFLAG
 30   FORMAT (T5,'Data dimension limit:',I6,'  INFLAG:',I3)
      ENDIF
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zritsx6', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
      CALL zinqir6 (IFLTAB, 'UNIT', CTEMP,IUNIT)
      LFOUND = .FALSE.
      LQREAD = .FALSE.
C
C
      IF (CPATH(1:1).NE."/") GO TO 910
C     Look for a pseudo regular time series pathname... The e part
C     starts with a tilde.  We treat this fully as an irregular
C     interval data set, but the user sees the general interval
      I = INDEX(CPATH(1:NPATH), '/~')
      IF (I.GT.2) THEN
        CALL ZPseudoRTS6(CPATHNAME(1:NPATH), CPATH, INTLPS, 1, ISTAT)
        IF (ISTAT.EQ.0) THEN
          CALL CHRLNB(CPATH, NPATH)
        ENDIF
      ENDIF
      CALL ZUPATH (CPATH, IBPART, IEPART, ILPART, ISTAT)
      IF (ISTAT.NE.0) GO TO 910
C
C
C     Check for 'Pattern' Time Series, data that
C     has no specific time associated with it
C     (such as a unit hydrograph or mean maximum daily temperatures)
C
      IF (ILPART(4).GT.3) THEN
         IF (CPATH(IBPART(4):IBPART(4)+2).EQ."TS-") THEN
            KB = KLBUFF
            CALL zreadx6 (IFLTAB, CPATH, IIHEAD, KIHEAD, NNIHEAD,
     *      IDUM, 0, N, IUHEAD, KUHEAD, NUHEAD, ILBUFF, KB, NB, 0,
     *      LFOUND)
            CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
            IF (JSTAT.NE.0) GO TO 940
            IF (.NOT.LFOUND) THEN
               CPATH1 = CPATH
               NVALS = 0
            ELSE
C
C              Move data from buff into values and dates
               NIHEAD = NNIHEAD
               NVALS = IIHEAD(2)
               IF (NVALS.GT.KVALS) NVALS = KVALS
               IF (LGETDOB) THEN
C              Time pattern data is always single precision.
                  DO 35 I = 1, NVALS
                    J = ((I-1)*2) + 1
                    ITIMES(I) = ILBUFF(J)
                    DVALUES(I) = BUFF(J+1)
 35               CONTINUE
               ELSE
                  DO 40 I = 1, NVALS
                    J = ((I-1)*2) + 1
                    ITIMES(I) = ILBUFF(J)
                    SVALUES(I) = BUFF(J+1)
 40               CONTINUE
               ENDIF
               IBDATE = IIHEAD(3)
               ISTAT = 0
            ENDIF
            LQREAD = .FALSE.
            GO TO 800
         ENDIF
      ENDIF
C
C
C     Check for reading just one record (no time window)
C
      IF (ISTIME.EQ.-2) THEN
C
      ISTAT = 0
      KB = KLBUFF
      CALL zreadx6 (IFLTAB, CPATH, IIHEAD, KIHEAD, NNIHEAD,
     * IDUM, 0, N, IUHEAD, KUHEAD, NUHEAD, ILBUFF, KB, NB, 2, LF)
      CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
      IF (JSTAT.NE.0) GO TO 940
C
      LFOUND = LF
C
      IF (LF) THEN
C
C     Check to be sure that the data is irregular-interval time series
      NIHEAD = NNIHEAD
      CALL zinqir6 (IFLTAB, 'TYPE', CSCRAT, IDTYPE)
      IF (IDTYPE.EQ.110) THEN
         LFILDOB = .FALSE.
      ELSE IF (IDTYPE.EQ.115) THEN
         LFILDOB = .TRUE.
      ELSE
         GO TO 930
      ENDIF
C
C     Write the message that the data was read
      IF (MLEVEL.GE.4) THEN
      CALL CHRLNB(CPATH, NPATH)
      IF (L80COL) THEN
      WRITE ( MUNIT,50) CPATH(1:NPATH)
 50   FORMAT(' --ZREAD:  ',A)
      ELSE
      CALL zinqir6(IFLTAB, 'RVER', CSCRAT, IVER)
      WRITE ( MUNIT,60) IFLTAB(KUNIT), IVER, CPATH(1:NPATH)
 60   FORMAT(' -----DSS---ZREAD Unit',I5,'; Vers.',I5,':',2X,A)
      ENDIF
      ENDIF
C
      LGETQ = .FALSE.
      CALL zinqir6 (IFLTAB, 'QUAL', CSCRAT, JQUAL)
      IF (JQUAL.EQ.1) THEN
      IMULT = 3
      IF (LQUAL) LGETQ = .TRUE.
      ELSE
      IMULT = 2
      CPATH1 = CPATH
      ENDIF
C
      IF (LFILDOB) THEN
         IMULT = IMULT + 1
      ENDIF
C
C     Check to be sure we retrieved a full block
      J = IIHEAD(2) * 2
C     Not enough room to move all the data into the array
      IF (KLBUFF.LT.J) GO TO 900
C
C     Move data from buff into values and dates
      NVALS = IIHEAD(2)
      IF (NVALS.GT.KVALS) THEN
      CPATH1 = CPATH
      GO TO 920
      ENDIF
      DO 70 I = 1, NVALS
      J = ((I-1)*IMULT) + 1
      IF (LGETDOB.EQV.LFILDOB) THEN
         CALL zircpy6 (ILBUFF(J), ITIMES(I), DVALUES, SVALUES,
     *        IQUAL, I, LFILDOB, LGETQ, .FALSE., .FALSE., LDSWAP)
      ELSE
         CALL zircpy6 (ILBUFF(J), ITIMES(I), DVAL, SVAL,
     *        IQ, 1, LFILDOB, LGETQ, .FALSE., .FALSE., LDSWAP)
         IF (LGETQ) IQUAL(I) = IQ(1)
         IF (LGETDOB) THEN
            DVALUES(I) = DBLE(SVAL(1))
         ELSE
            SVALUES(I) = SNGL(DVAL(1))
         ENDIF
      ENDIF
 70   CONTINUE
C
      IBDATE = IIHEAD(3)
C
      ENDIF
C
      ELSE
C
C
C     A time window was specified - read blocks
C
C     set next and previous variables
      IFORWD = 1
      LCASE = .FALSE.
      LPREV = .FALSE.
      LNEXT = .FALSE.
      IF ((INFLAG .EQ. 1) .OR. (INFLAG .EQ. 3)) LPREV = .TRUE.
      IF ((INFLAG .EQ. 2) .OR. (INFLAG .EQ. 3)) LNEXT = .TRUE.
C
      IBDATE = JULS
      NVALS = 0
      ISTAT = 0
C
C
      CALL zupath (CPATH, IBPART, IEPART, ILPART, ISTAT)
      IF (CPATH(1:1).NE."/") GO TO 910
      IF (ISTAT.EQ.0) CALL zirbeg6 (IFLTAB, JULS,
     * CPATH(IBPART(5):IEPART(5)), IYR, IMON, IDAY, IBLOCK, MINBLK,
     * INCBLK)
C
      IF ((IBLOCK.LE.0).OR.(ISTAT.NE.0)) GO TO 910
C
C     Get starting date of first block
      JUL = IYMDJL (IYR, IMON, IDAY)
      IF (ILPART(6).GT.0) THEN
         CPATH1 = CPATH(1:IBPART(4)-1) // '01JAN1900/' //
     *            CPATH(IBPART(5):IEPART(6)+1)
      ELSE
         CPATH1 = CPATH(1:IBPART(4)-1) // '01JAN1900/' //
     *            CPATH(IBPART(5):IEPART(6))
      ENDIF
      CALL CHRLNB (CPATH1, NPATH)
      GO TO 200
C
C
 120  CONTINUE
C
C     Get dates of subsequent blocks
C
      IF (IBLOCK.EQ.1) THEN
C     Increment by day
      JUL = IYMDJL (IYR, IMON, IDAY) + IFORWD
      K = JLIYMD( JUL, IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.2) THEN
C     Increment by month
      IMON = IMON + IFORWD
      IF ((IMON .LT. 1) .OR. (IMON .GT. 12)) THEN
      IYR = IYR + IFORWD
      IMON = IMON - 12*IFORWD
      ENDIF
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.3) THEN
C     Increment by year
      IYR = IYR + IFORWD
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.4) THEN
C     Increment by decade
      IYR = IYR + 10*IFORWD
      JUL = IYMDJL (IYR, IMON, IDAY)
C
      ELSE IF (IBLOCK.EQ.5) THEN
C     Increment by decade
      IYR = IYR + 100*IFORWD
      JUL = IYMDJL (IYR, IMON, IDAY)
      ENDIF
C
C     set IFORWD to increment
C
 200  CONTINUE
      IFORWD = 1
C
      IF (MLEVEL.GE.9) WRITE (MUNIT, 201) JULE, JUL
 201  FORMAT (T5,'At 200, JULE, JUL: ',2I8)
      IF ((MLEVEL.GE.9).AND.(INFLAG.NE.0)) WRITE (MUNIT, 202)
     * LPREV, LNEXT, LCASE
 202  FORMAT (T5,'LPREV:',L2,'  LNEXT:',L2,'  LCASE:',L2)
C
C     have we reached the end of the time window - may get the next
C     value if provided the time window isn't empty
      IF ((JULE.LT.JUL).AND.(.NOT.LNEXT).AND.(.NOT.LPREV)) GO TO 800
C
C     FORM THE PATHNAME
      CALL JULDAT (JUL, 104, CRDATE, NDATE1)
      CPATH1(IBPART(4):IBPART(4)+8) = CRDATE
C
      KB = KLBUFF
      CALL zreadx6 (IFLTAB, CPATH1, IIHEAD, KIHEAD, NNIHEAD,
     * IDUM, 0, N, IUHEAD, KUHEAD, NUHEAD, ILBUFF, KB, NB, 2, LF)
      CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
      IF (JSTAT.NE.0) GO TO 940
C
C     record not found
      IF (.NOT.LF) THEN
C
C     is the first record found is empty - go to the next one
      IF (LPREV) THEN
      LCASE = .TRUE.
      LPREV = .FALSE.
      IFORWD = -1
      GOTO 120
      ENDIF
C
C     previous value not found - first data point is missing
      IF (LCASE) THEN
      LCASE = .FALSE.
C     go to the next (initial) record
      GOTO 120
C
C     next value not found - test for overflow
      ELSE IF (LNEXT) THEN
C
C     if the time series has initial empty intervals do not end
      IF (JUL .LE. JULE) THEN
C     if the series is missing an interval in the middle
      GOTO 120
      ELSE
      GOTO 800
      ENDIF
C
C     missing record is within the time window
      ELSE
      GO TO 120
      ENDIF
      ENDIF
C
C
C     A valid record was read
      LFOUND = .TRUE.
C
C     Check to be sure that the data is irregular-interval time series
      NIHEAD = NNIHEAD
      CALL zinqir6 (IFLTAB, 'TYPE', CSCRAT, IDTYPE)
      IF (IDTYPE.EQ.110) THEN
         LFILDOB = .FALSE.
      ELSE IF (IDTYPE.EQ.115) THEN
         LFILDOB = .TRUE.
      ELSE
         GO TO 930
      ENDIF
C
      LGETQ = .FALSE.
      CALL zinqir6 (IFLTAB, 'QUAL', CSCRAT, JQUAL)
      IF (JQUAL.EQ.1) THEN
      IMULT = 3
      IF (LQUAL) LGETQ = .TRUE.
      ELSE
      IMULT = 2
      ENDIF
C
      IF (LFILDOB) THEN
         IMULT = IMULT + 1
      ENDIF
C
C     Check to be sure we retrieved a full block
      J = IIHEAD(2) * 2
C     Not enough room to move all the data into the array
      IF (KLBUFF.LT.J) GO TO 900
C
C     MOVE BUFF INTO DATES AND VALUES ARRAY
C
      JDATE = IIHEAD(3)
      JTIME = ILBUFF(1)
C
C     previous value not in the block
      IF (LPREV) THEN
      JTIME = ISTIME
      IF (MLEVEL.GE.9) THEN
      N= ICTIME (JDATE, ILBUFF(1), IBDATE, JTIME)
      WRITE (MUNIT, 305) JDATE, ILBUFF(1), IBDATE, JTIME, N
 305  FORMAT (T5,'LPREV TRUE, JDATE, ILBUFF(1), IBDATE, JTIME, ICTIME:',
     * /,T15,5I8)
      ENDIF
      IF (ICTIME (JDATE, ILBUFF(1), IBDATE, JTIME).GE.0) THEN
      LCASE = .TRUE.
      LPREV = .FALSE.
      IFORWD = -1
      GOTO 120
      ENDIF
      ENDIF
C
C     get the last value from the record - special case
      IF (LCASE) THEN
      LCASE = .FALSE.
      IBDATE = IIHEAD(3)
      N = ((IIHEAD(2) - 1) * IMULT) + 1
      IF (LGETDOB.EQV.LFILDOB) THEN
         CALL zircpy6 (ILBUFF(N), ITIMES(1), DVALUES, SVALUES,
     *        IQUAL, 1, LFILDOB, LGETQ, .FALSE., .FALSE., LDSWAP)
      ELSE
         CALL zircpy6 (ILBUFF(N), ITIMES(1), DVAL, SVAL,
     *        IQ, 1, LFILDOB, LGETQ, .FALSE., .FALSE., LDSWAP)
         IF (LGETQ) IQUAL(1) = IQ(1)
         IF (LGETDOB) THEN
            DVALUES(1) = DBLE(SVAL(1))
         ELSE
            SVALUES(1) = SNGL(DVAL(1))
         ENDIF
      ENDIF
      NVALS = 1
      GOTO 120
      ENDIF
C
C     LOOK FOR FIRST VALUE IN OUR TIME WINDOW
      DO 310 I=1,IIHEAD(2)
      IARY = I
      J = ((I-1) * IMULT) + 1
      JTIME = ISTIME
      IF (ICTIME (JDATE, ILBUFF(J), JULS, JTIME).GE.0) GO TO 350
 310  CONTINUE
C
C     Found no requested data in this record
      GO TO 120
C
C     get the previous value, and turn off LPREV
 350  CONTINUE
      IF (LPREV) THEN
      LPREV = .FALSE.
      IARY = IARY - 1
      ENDIF
C
C     NOW MOVE DATA FROM BUFF INTO VALUES AND DATES
      DO 370 I = IARY, IIHEAD(2)
C     if LNEXT is set read one more value
      J = ((I-1) * IMULT) + 1
      JTIME = IETIME
      IF ((ICTIME (JDATE, ILBUFF(J), JULE, JTIME).GT.0).AND.
     * (.NOT.LNEXT)) GO TO 800
      NVALS = NVALS + 1
      IF (NVALS.GT.KVALS) GO TO 920
      IF (LGETDOB.EQV.LFILDOB) THEN
         CALL zircpy6 (ILBUFF(J), IT, DVALUES, SVALUES,
     *        IQUAL, NVALS, LFILDOB, LGETQ, .FALSE., .FALSE., LDSWAP)
      ELSE
         CALL zircpy6 (ILBUFF(J), IT, DVAL, SVAL,
     *               IQ, 1, LFILDOB, LGETQ, .FALSE., .FALSE., LDSWAP)
         IF (LGETQ) IQUAL(NVALS) = IQ(1)
         IF (LGETDOB) THEN
            DVALUES(NVALS) = DBLE(SVAL(1))
         ELSE
            SVALUES(NVALS) = SNGL(DVAL(1))
         ENDIF
      ENDIF
      I6 = JDATE - IBDATE
      ITIMES(NVALS) = (I6 * 1440) + IT
C     only executed if LNEXT was true above
      JTIME = IETIME
      IF (ICTIME (JDATE, ILBUFF(J), JULE, JTIME).GT.0) GO TO 800
 370  CONTINUE
C
C     Go back and read next record
C
      GO TO 120
C
      ENDIF
C
C
C     Completed
 800  CONTINUE
      IF ((NVALS.EQ.0).AND.(ISTAT.EQ.0)) ISTAT = 3
      IF (LQUAL) THEN
      LQREAD = .FALSE.
      IF (LGETQ) LQREAD = .TRUE.
      ENDIF
C
      IF (.NOT.LFOUND) THEN
      CALL CHRLNB(CPATH1,N)
      IF (MLEVEL.GE.2) WRITE (MUNIT,810) IUNIT, CPATH1(1:N)
 810  FORMAT(' -----DSS*** zrits6:  Caution - Missing Block of Data;',
     * ' Unit:',I5,/,' Pathname: ',A)
      NVALS = 0
      IBDATE = 0
      ISTAT = 4
C
      ELSE
C
C     MOVE HEADER INFORMATION INTO ARGUMENTS
      CALL HOLCHR (IIHEAD(6),  1, 8, CSCRAT, 1)
      call strcpy(CUNITS, CSCRAT)
      CALL HOLCHR (IIHEAD(8), 1, 8, CSCRAT,  1)
      call strcpy(CTYPE, CSCRAT)
      IF (NIHEAD.GE.10) THEN
         IRTZONE = IIHEAD(10)
      ELSE
         IRTZONE = -1
      ENDIF
      call strcpy(CRTZONE, ' ')
      IF (NIHEAD.EQ.13) THEN
         CALL HOLCHR (IIHEAD(11), 1, 12, CRTZONE,  1)
      ELSE IF (NIHEAD.GE.16) THEN
         CALL HOLCHR (IIHEAD(11), 1, 24, CRTZONE,  1)
         IF (NIHEAD.GE.28) THEN
	      CALL zmovwd6(IIHEAD(17), COORDS(1), 6)
            DO 811 I=1,6
               ICDESC(I) = IIHEAD(I+22)
 811        CONTINUE
            DO 812 I=1,3
	         IF (COORDS(I).NE.0.0) LCOORDS = .TRUE.
 812        CONTINUE
         ENDIF
      ENDIF
C
      ENDIF
C
C
 820  CONTINUE
      IF (MLEVEL.GE.9) THEN
      WRITE (MUNIT, 840) NVALS, ISTAT
 840  FORMAT (T5,'----- Exiting zrits6;  Number of Data:',I7,
     * ',  Status:',I4)
C
      IF ((NVALS .LT. KVALS) .AND.
     +(NVALS .GT. 0)) THEN
C
      CALL DATCLL (IBDATE, ITIMES(1), JLS, IST)
      CALL JULDAT (JLS, 104, CDATE1, NDATE1)
      CALL DATCLL (IBDATE, ITIMES(NVALS), JLE, IET)
      CALL JULDAT (JLE, 104, CDATE2, NDATE2)
C
      WRITE (MUNIT, 860) JLS, IST, CDATE1, JLE, IET, CDATE2, IBDATE
 860  FORMAT (T10,'Date of first value: ',2I8,2X,A,
     * /,T10,'Date of Last value:  ',2I8,2X,A,
     * /,T10,'Base date: ',I8,/)
C
      ENDIF
      ENDIF
C
      !---------------------------------------------------------!
      ! convert values to requested vertical datum if necessary !
      !---------------------------------------------------------!
      call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne, cf, nf,
     *           cpath, len_trim(cpath), istat)
      call upcase(cc)
      if (index(cc, 'ELEV').eq.1) then
        !--------------------------!
        ! time series is elevation !
        !--------------------------!
        call zinqir(ifltab, 'VDTM', cvdatum, ivdatum)
        if (ivdatum.ne.IVD_UNSET) then
          !-----------------------------------------!
          ! a specific vertical datum was requested !
          !-----------------------------------------!
          if (nuhead.gt.0) then
            call get_user_header_param(
     *        iuhead,
     *        nuhead,
     *        VERTICAL_DATUM_INFO_PARAM,
     *        vdiStr)
            vdiStrLen = len_trim(vdiStr)
            if (vdiStrLen.gt.0) then
              !-----------------------------------------------------------!
              ! we retrieved a user header and it has vertical datum info !
              !-----------------------------------------------------------!
              call stringToVerticalDatumInfo(
     *          vdiStr,
     *          errMsg,
     *          nativeDatum,
     *          unit,
     *          offsetNgvd29,
     *          l_Ngvd29Estimated,
     *          offsetNavd88,
     *          l_Navd88Estimated)
              !--------------------------------------------!
              ! add the requested datum to the user header !
              !--------------------------------------------!
              iuhead(nuhead+1:kuhead) = 0
              call set_user_header_param(iuhead, nuhead, kuhead, 
     *          VERTICAL_DATUM_PARAM, cvdatum, istat)
              if (istat.ne.0) then
                if (mlevel.ge.1) then
                    write (munit,'(/,a,a,/,a)')
     *              ' *****DSS*** zrits6:  ERROR  - VERTICAL DATUM',
     *              ' TRUNCATED',
     *              ' No values retrieved.'
                  end if
                  istat = 13
                  return
                end if
              !--------------------------------------!
              ! get the vertical datum offset to use !
              !--------------------------------------!
              if (ivdatum.eq.IVD_NAVD88) then
                vertDatumOffset = offsetNavd88
              elseif (ivdatum.eq.IVD_NGVD29) then
                vertDatumOffset = offsetNgvd29
              else
                if (nativeDatum.eq.cvdatum.or.
     *              nativeDatum.eq.CVD_OTHER) then
                  vertDatumOffset = 0.
                else
                  vertDatumOffset = UNDEFINED_VERTICAL_DATUM_VALUE
                end if
              end if
              if (vertDatumOffset.ne.0) then
                if (vertDatumOffset.eq.
     *            UNDEFINED_VERTICAL_DATUM_VALUE) then
                  if (mlevel.ge.1) then
                    write (munit,'(/,a,a,a,a,a,/,a)')
     *              ' *****DSS*** zrits6:  ERROR  - NO VERTICAL DATUM',
     *              ' OFFSET for ',nativeDatum(1:len_trim(nativeDatum)),
     *              ' to ',cvdatum(1:len_trim(cvdatum)),
     *              ' No values retrieved.'
                  end if
                  istat = 13
                  return
                end if
                !------------------------------------------------------------!
                ! convert the vertical datum offset to the units of the data !
                !------------------------------------------------------------!
                call getoffset(vertDatumOffset, unit, cunits)
                if (vertDatumOffset.eq.
     *            UNDEFINED_VERTICAL_DATUM_VALUE)then
                  if (mlevel.ge.1) then
                    write (munit,'(/,a,a,a,a,a,a,a,/,a)')
     *              ' *****DSS*** zrits6:  ERROR  - INVALID DATA UNIT ',
     *              '(',cunits(1:len_trim(cunits)),') OR OFFSET UNIT (',
     *              unit(1:len_trim(unit)),') FOR VERTICAL DATUM',
     *              ' CONVERSION',
     *              ' No values retrieved.'
                  end if
                  istat = 13
                  return
                end if
                !-----------------------------------!
                ! add the offset to the data values !
                !-----------------------------------!
                if (lgetdob) then
                  do i = 1, nvals
                    if (dvalues(i).ne.-901.and.dvalues(i).ne.-902) then
                      dvalues(i) = dvalues(i) + vertdatumoffset
                    end if
                  end do
                else
                  do i = 1, nvals
                    if (svalues(i).ne.-901.and.svalues(i).ne.-902) then
                      svalues(i) = svalues(i) + vertdatumoffset
                    end if
                  end do
                end if
              end if
            end if
          end if
        end if
      end if
      RETURN
C
C
C     ERROR STATEMENTS
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901) KLBUFF, J, CPATH1(1:NPATH)
 901  FORMAT(/' ***** Error:  zrits6:  Buffer size not large enough',/,
     * ' to read the record specified',/,' Buffer size:',I6,
     * '  Record size:',I6,/,' Pathname: ',A,/)
      ISTAT = 21
      GO TO 800
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) CPATH1(1:NPATH)
 911  FORMAT(' -----DSS*** zrits6;  ERROR:  Unable to Recognize',
     * ' Pathname as Irregular Time-Series',/,
     * ' Pathname: ',A)
      ISTAT = 24
      GO TO 820
C
 920  CONTINUE
      CALL CHRLNB (CPATH1, NPATH)
      IF ((MLEVEL.GE.3).AND.(IKSTAT.NE.-1))
     * WRITE (MUNIT,921) KVALS, CPATH1(1:NPATH)
 921  FORMAT(' ***** WARNING - zrits6;  Number of Data Found Exceeds',
     * ' Dimension Limit;',/,T26,'Reading Terminated *****',/,
     * ' Dimension Limit: ',I6,/,' Pathname: ',A)
      ISTAT = 1
      NVALS = KVALS
      GO TO 800
C
 930  CONTINUE
      CALL CHRLNB(CPATH,N)
      IF (MLEVEL.GE.1) WRITE (MUNIT,931) CPATH(1:N), IDTYPE
 931  FORMAT (/,' *****DSS*** zrits6:  ERROR  - The Data is not',
     * ' Irregular-Interval Time Series',/,' Pathname: ',A,/,
     * ' Data Type:',I5,/)
      ISTAT = 20
      GO TO 820
C
 940  CONTINUE
      CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
      ISTAT = JSTAT
      CALL CHRLNB(CPATH,N)
      IF (MLEVEL.GE.1) WRITE (MUNIT,941) ISTAT, CPATH(1:N)
 941  FORMAT (/,' *****DSS*** zrits6:  ERROR  - UNABLE TO ',
     * ' RETRIEVE DATA',/,' Status: ',I8,/,' Pathname: ',A,/)
      NVALS = 0
      RETURN
C
      END

