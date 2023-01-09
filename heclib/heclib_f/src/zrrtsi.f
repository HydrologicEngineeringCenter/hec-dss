      SUBROUTINE zrrtsi6 (IFLTAB, CPATH, CRDATE, CRTIME, KVALS, NVALS,
     * LGETDOB, LFILDOB, SVALUES, DVALUES, JQUAL, LQUAL, LQREAD,
     * CUNITS, CTYPE, IUHEAD, KUHEAD, NUHEAD, IOFSET, JCOMP,
     * COORDS, ICDESC, LCOORDS, ISTAT)
C
C
C     Z-Retrieve Regular interval Time-Series data internal
C
C     Input:
C        IFLTAB:  Working DSS array used in zopen6 call.  Must be
C                 be dimensioned as INTEGER with 1200 words
C        CPATH:   Pathname of the data to be retrieved.  If a time
C                 window is specified (CRDATE and CRTIME), the 'D'
C                 part is ignored;  Otherwise all parts must be
C                 correct.  CPATH sould be declared as CHARACTER*80
C        CRDATE:  Beginning date of the time window.  This may be
C                 a standard military style date (e.g. 01MAR74).
C                 If the data is to be retrieved without a time window
C                 (i.e. all data specified by pathname), set CRDATE to
C                 all blanks, or make length short (e.g. ' ').
C        CRTIME:  Beginning time of the time window.  This must be
C                 a standard 24 hour clock time (e.g. 1630).  If no
C                 time window is set, this parameter is ignored.
C                 CRTIME should be declared as CHARACTER*4.
C        KVALS:   The number of data values to retrieve.  This parameter
C                 defines the end of the time window.  If the entire
C                 record is to be retrived (CRDATE equal ' '), then
C                 SVALUES must be dimensioned large enough to
C                 hold all the data in the record, and KVALS should be
C                 the dimension of the array SVALUES (or DVALUES).
C        LGETDOB: Logical set to TRUE if the data should be returned
C                 as DOUBLES in array DVALUES (SVALUES ignored), or
C                 FALSE if the data is to be returned as REALS in
C                 array SVALUES (DVALUES ignored).
C        ISTAT:   If set to -1, don't print the zread6 message.
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
C        IOFSET:  The time offset of the data in minutes.  If there
C                 is no offset, IOFSET is returned as zero.
C        ISTAT:   Integer status parameter, indicating the
C                 successfullness of the retrieval.
C                 ISTAT = 0  All ok.
C                 ISTAT = 1  Some missing data (still ok)
C                 ISTAT = 2  Missing data blocks, but some data found
C                 ISTAT = 3  Combination of 1 and 2 (some data found)
C                 ISTAT = 4  No data found, although a pathname was read
C                 ISTAT = 5  No pathname(s) found
C                 ISTAT > 9  Illegal call to zrrts6
C
C     Written by Bill Charley
C
C
C     DIMENSION STATEMENTS
C
C     Argument Dimensions
      CHARACTER CPATH*(*), CTYPE*(*), CUNITS*(*), CRDATE*(*), CRTIME*(*)
      INTEGER IFLTAB(*), IUHEAD(*), JQUAL(*), ICDESC(*)
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*), COORDS(*)
      LOGICAL LCOORDS
C
      include 'dss_parameters.h'

C     Local Dimensions
      INTEGER IBPART(6), IEPART(6), ILPART(6)
      CHARACTER(len=dss_maxpath) CTSPAT
      CHARACTER CDATE1*12, CDATE2*12, CTIME1*4, CTIME2*4, CSCRAT*8
      LOGICAL LFOUND, LTIMEW, LPATH, LQUAL, LQREAD, LQBLOK
      LOGICAL LGETDOB, LFILDOB
	PARAMETER (KIHEAD = 24)
      INTEGER IIHEAD(KIHEAD)
C
C     Vertical datum varible dimensions
      character*400 vdiStr, errMsg
      character*16 nativeDatum, unit, cvdatum1, prevVerticalDatum
      double precision offsetNavd88, offsetNgvd29, vertDatumOffset
      logical l_Navd88Estimated, l_Ngvd29Estimated
      integer vdiStrLen, paramIsElev
C
      data lqblok /.false./


      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'verticalDatumFortran.h'
C
C
C
      NSTART = 1
      ISTAT = 0
      JCOMP = 0
      NUHEAD = 0
      IQUAL = 0
      LPATH = .FALSE.
      LQREAD = .FALSE.
      LFILDOB = .FALSE.
      LCOORDS = .FALSE.
      prevVerticalDatum = 'never set'
C
      IFUNIT = IFLTAB(KUNIT)
C
C
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) THEN
      WRITE (MUNIT,20) IFUNIT
 20   FORMAT (T10,'----- Entering zrrtsx6 for unit',I5,' -----')
      ENDIF
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zrrtsx6', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
C
C     Unform the pathname
      CALL CHRLNB (CPATH, NPATH)
      IF ((NPATH.GT.dss_maxpath).OR.(NPATH.LE.1)) GO TO 900
      IF (CPATH(1:1).NE.'/') GO TO 900
      CALL zupath (CPATH, IBPART, IEPART, ILPART, IERR)
      IF (IERR.NE.0) GO TO 900
      IF (ILPART(5).LT.4) GO TO 960
C
      IF (MLEVEL.GE.7) WRITE (MUNIT, 40) CPATH(1:NPATH)
 40   FORMAT (' Pathname: ',A)
C
C     Determine the time, interval in minutes
      I = 1
      CALL zgintl6 (INTL, CPATH(IBPART(5):IEPART(5)), NUMDAT, I)
      IF (I.LT.0) GO TO 910
C
C     See if a time window has been set
      LTIMEW = .FALSE.
      I = LEN(CRDATE)
      IF (I.GT.4) THEN
      J = NINDX (CRDATE,' ')
      IF (J.NE.0) LTIMEW = .TRUE.
      ENDIF
C
C     Check that a positive number of data was requested
      IF (KVALS.LE.0) GO TO 920
C
C     Check for 'Pattern' Time Series, data that
C     has no specific time associated with it
C     (such as a unit hydrograph or mean maximum daily temperatures)
C
      IF (ILPART(4).GT.3) THEN
         CALL UPCASE (CPATH(IBPART(4):IEPART(4)))
         IF (CPATH(IBPART(4):IBPART(4)+2).EQ."TS-") THEN
            IF (LGETDOB) THEN
C              Time pattern data is always single precision.
               NVALS = KLBUFF
               CALL zrrtpa6 (IFLTAB, CPATH, BUFF, NVALS, CUNITS,
     *                      CTYPE, IUHEAD, KHEADU, NUHEAD, ISTAT)
               DO 50 I=1,NVALS
                   DVALUES(I) = BUFF(I)
 50            CONTINUE
            ELSE
               NVALS = KVALS
               CALL zrrtpa6 (IFLTAB, CPATH, SVALUES, NVALS, CUNITS,
     *                      CTYPE, IUHEAD, KHEADU, NUHEAD, ISTAT)
            ENDIF
            LCOORDS = .FALSE.
            LQREAD = .FALSE.
            IOFSET = 0
            JCOMP = 0
            GO TO 800
         ENDIF
      ENDIF
C
      IF (.NOT.LTIMEW) THEN
      IF (MLEVEL.GE.7) WRITE (MUNIT,60) INTL, KVALS
 60   FORMAT (T10,'No time Window set.  Interval:',I7,'  Maximum',
     * ' number of data values:',I8,/,T10,'Computed Maximum Times:')
      IF (KVALS.LT.NUMDAT) THEN
      IF (MLEVEL.GE.5) WRITE (MUNIT,70) CPATH(1:NPATH), KVALS, NUMDAT
 70   FORMAT (' -----DSS--- zrrts6:  Warning - More data available',
     * ' than requested',/,' Pathname: ',A,/,' Number requested:',I5,
     * '  Number Available:',I5)
      ENDIF
C
C     If no time window set, compute the starting date and time
      IF (ILPART(4).LT.5) GO TO 900
      CALL DATJUL (CPATH(IBPART(4):IEPART(4)), JULS, IERR)
      IF (IERR.NE.0) GO TO 900
      ISTIME = 0
      I = INCTIM (INTL, 0, 1, JULS, ISTIME, JULS, ISTIME)
      NVALS = MIN0 (KVALS,NUMDAT)
C
      ELSE
C
C     Time window set - compute julian dates and times
      NVALS = KVALS
      CALL DATJUL ( CRDATE, JULS, IERR)
      IF (IERR.NE.0) GO TO 930
      ISTIME = IHM2M (CRTIME)
      IF (ISTIME.EQ.0) THEN
      JULS = JULS - 1
      ISTIME = 1440
      ENDIF
C     Check for an illegal starting time
      IF ((ISTIME.LT.0).OR.(ISTIME.GT.1440)) GO TO 940
C     If the time is not on the standard boundaries, adjust it
      CALL zofset6 (JULS,ISTIME,INTL,1,IOFF)
C
      ENDIF
C
C     Compute the ending time from the number of values desired
      I = INCTIM (INTL, 0, KVALS-1, JULS, ISTIME, JULE, IETIME)
C
      IF (MLEVEL.GE.7) THEN
      IF (LTIMEW) WRITE (MUNIT,80)INTL,KVALS
 80   FORMAT (T10,'Time Window set.  Interval:',I6,'  Number of',
     * ' data values:',I7)
      CALL JULDAT (JULS, 1, CDATE1, NDATE1)
      CALL JULDAT (JULE, 1, CDATE2, NDATE2)
      I = M2IHM (ISTIME, CTIME1)
      I = M2IHM (IETIME, CTIME2)
      WRITE (MUNIT,90) CDATE1(1:NDATE1), CTIME1, JULS, ISTIME,
     * CDATE2(1:NDATE2), CTIME2, JULE, IETIME, IOFF
 90   FORMAT (T10,'Starting date and time:  ',A,2X,A,'  (',I7,I5,')',/,
     *        T10,'Ending   date and time:  ',A,2X,A,'  (',I7,I5,')',/,
     *        T10,'Input time offset:',I7)
      ENDIF
C
C
C
C     Obtain the date of the first block
      CALL zbegdt6 (JULS, INTL, IYR, IMON, IDAY, IBLOCK, IFLTAB(KVERNO))
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
C     Get the date of the last block
      CALL zbegdt6 (JULE, INTL, JYR, JMON, JDAY, IBLOCK, IFLTAB(KVERNO))
      JULAST = IYMDJL (JYR, JMON, JDAY)
C
C
C     Loop, reading data blocks, changing the D (date)
C     part of the pathname each time
C
      CDATE1 = ' '
 100  CONTINUE
C     Get the new D (Date) part
      CALL YMDDAT (IYR, IMON, IDAY, 104, CDATE1, NDATE1, IERR)
      IF (IERR.NE.0) GO TO 950
      JULSD = IYMDJL (IYR, IMON, IDAY)
C
      N = IEPART(3) + 1
      IF (ILPART(3).EQ.0) N = IEPART(3)
      CTSPAT = CPATH(1:N) // CDATE1(1:NDATE1) //
     * CPATH(IBPART(5)-1:NPATH)
      CALL CHRLNB (CTSPAT, NTSPAT)
C
C
C     Get the time of the next data block
      CALL zincbk6 (IBLOCK, JUL, IYR, IMON, IDAY)
C
      IF (INTL.EQ.10080) THEN
C     Weekly data.  Set so that the data is always on
C     Saturday at 2400 hours.
      NDAY = IDAYWK (JUL)
      JUL = JUL - NDAY + 1
      NDAY = IDAYWK (JULSD)
      JULSD = JULSD - NDAY + 1
      ENDIF
C
C
C     Determine if this record exists.
C     If it does, read its information block.
C
      CALL zrdinf6 (IFLTAB, CTSPAT(1:NTSPAT), NHEAD, NADATA, JSTAT)
      IF (JSTAT.EQ.0) THEN
      IF ((IFLTAB(KDTYPE).NE.100).AND.(IFLTAB(KDTYPE).NE.105)) GO TO 970
      IF (INFO(NPPWRD+KITYPE).EQ.100) THEN
         LFILDOB = .FALSE.
      ELSE
         LFILDOB = .TRUE.
      ENDIF
      LFOUND = .TRUE.
      ELSE
      LFOUND = .FALSE.
      ENDIF
C
      IF (MLEVEL.GE.8) THEN
      CALL CHRLNB(CTSPAT,N)
      WRITE (MUNIT,220) LFOUND, CTSPAT(1:N)
 220  FORMAT(T10,'After zrdinf, Record found:',L2,/,T10,'Pathname: ',A)
      IF (LFOUND) THEN
      WRITE (MUNIT,230) NADATA, NHEAD, INFO(NPPWRD+KICOMP),
     * INFO(NPPWRD+KIQUAL), LFILDOB
 230  FORMAT (T10,'Number of actual data:',I5,'  Header length:',I4,/,
     * T10,'Compression:',I4,'  Quality:',I4,'  Double:',L2)
      ENDIF
      ENDIF
C
      IF (LFOUND) THEN
C
C     Write the message that the data was read
      IF (MLEVEL.GE.4) THEN
      IF (L80COL) THEN
      WRITE ( MUNIT,231) CTSPAT(1:NTSPAT)
 231  FORMAT(' --ZREAD:  ',A)
      ELSE
      WRITE ( MUNIT,232) IFLTAB(KUNIT), INFO(NPPWRD+KIVER),
     * CTSPAT(1:NTSPAT)
 232  FORMAT(' -----DSS---ZREAD Unit',I5,'; Vers.',I5,':',2X,A)
      ENDIF
      ENDIF
C
C
C     Determine the number of logical data values (which is different
C     when data compression or quality flags are used)
      NLDATA = INFO(NPPWRD+KILNDA)
C
C     Determine if the data quality flag is set
      IF (INFO(NPPWRD+KIQUAL).EQ.0) THEN
      LQBLOK = .FALSE.
      ELSE
      LQBLOK = .TRUE.
      ENDIF
C
C     Read the internal header (containing offset, units, etc.)
      NIHEAD = INFO(NPPWRD+KINIHE)
      IF (NIHEAD.GT.0)
     * CALL zgtrec6(IFLTAB, IIHEAD, NIHEAD, INFO(NPPWRD+KIAIHE), .TRUE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 979
C
C     If there is a user header, retrieve that
      NUHEAD = INFO(NPPWRD+KINUHE)
      NUHEAD = MIN0 (NUHEAD, KUHEAD)
      IF (NUHEAD.GT.0)
     *CALL zgtrec6(IFLTAB, IUHEAD, NUHEAD, INFO(NPPWRD+KIAUHE), .TRUE.)
      IF (IFLTAB(KSTAT).NE.0) GO TO 979
      call get_user_header_param(
     *  iuhead,
     *  nuhead,
     *  VERTICAL_DATUM_INFO_PARAM,
     *  vdiStr)
      if (len_trim(vdiStr).gt.0) then
        call stringToVerticalDatumInfo(
     *    vdiStr,
     *    errMsg,
     *    nativeDatum,
     *    unit,
     *    offsetNgvd29,
     *    l_Ngvd29Estimated,
     *    offsetNavd88,
     *    l_Navd88Estimated)
      end if
C
      ELSE
C
C     Record not found - need to compute the number of data that would
C     have been read
      NLDATA = NOPERS (INTL, 0, JULSD, 0, JUL, 0)
C
      ENDIF
C
C     If there is no time window, set the number of data
      IF (.NOT.LTIMEW) THEN
      NVALS = MIN0 (NLDATA,KVALS)
      !NVALS = NLDATA
      ENDIF
      !----------------------------------------------------------!
      ! save/compute data we will need after the call to zrrtsb6 !
      !----------------------------------------------------------!
      nstart_b4 = nstart
      npos = nopers (intl, 0, julsd, 0, juls, istime) + nstart_b4 - 1
      nread = nldata - npos + 1
      if (nvals - nstart + 1.lt.nread) nread = nvals - nstart_b4 + 1
C
C     Now retrieve the data for this record
C
      CALL zrrtsb6 (IFLTAB, JULS, ISTIME, INTL, JULSD, NSTART,
     * NLDATA, NADATA, NVALS, LGETDOB, LFILDOB, SVALUES, DVALUES,
     * JQUAL, LQBLOK, LQUAL, JCOMP, LFOUND, ISTAT)
      IF (IFLTAB(KSTAT).NE.0) GO TO 979
      IF (ISTAT.GT.9) GO TO 980
      !---------------------------------------------------------!
      ! convert values to requested vertical datum if necessary !
      !---------------------------------------------------------!
      call pathnameIsElevTs(cpath, paramIsElev)
      if (paramIsElev.eq.1) then
        !--------------------------!
        ! time series is elevation !
        !--------------------------!
        call zinqir(ifltab, 'VDTM', cvdatum1, ivdatum1)
        if (ivdatum1.eq.IVD_UNSET) then
          !--------------------------------------------------------------!
          ! look for and warn about multiple vertical datums in data set !
          !--------------------------------------------------------------!
          if (nuhead.gt.0) then
            call get_user_header_param(
     *        iuhead,
     *        nuhead,
     *        VERTICAL_DATUM_INFO_PARAM,
     *        vdiStr)
            vdiStrLen = len_trim(vdiStr)
            if (vdiStrLen.eq.0) then
              nativeDatum = ' '
            else
              call stringToVerticalDatumInfo(
     *          vdiStr,
     *          errMsg,
     *          nativeDatum,
     *          unit,
     *          offsetNgvd29,
     *          l_Ngvd29Estimated,
     *          offsetNavd88,
     *          l_Navd88Estimated)
            end if
            if (prevVerticalDatum.eq.'never set') then
              prevVerticalDatum = nativeDatum
            end if
            if (prevVerticalDatum.ne.nativeDatum      .and.
     *          prevVerticalDatum.ne.'already warned' .and.
     *          mlevel.ge.1) then
                  write (munit,'(/,a,/,a)')
     *            ' *****DSS*** zrits6:  WARNING  - VERTICAL DATUM'//
     *            ' CONFLICT',
     *            ' Elevation values are in multiple native vertical'//
     *            ' datums.',
     *            ' Use with caution!'
            end if
          end if
        else
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
     *          VERTICAL_DATUM_PARAM, cvdatum1, iistat)
              if (iistat.ne.0) then
                if (mlevel.ge.1) then
                    write (munit,'(/,a,a,/,a)')
     *              ' *****DSS*** zrrtsi6:  ERROR  - VERTICAL DATUM',
     *              ' TRUNCATED',
     *              ' No values retrieved.'
                end if
              end if
              !--------------------------------------!
              ! get the vertical datum offset to use !
              !--------------------------------------!
              if (ivdatum1.eq.IVD_NAVD88) then
                vertDatumOffset = offsetNavd88
              elseif (ivdatum1.eq.IVD_NGVD29) then
                vertDatumOffset = offsetNgvd29
              else
                if (nativeDatum.eq.cvdatum1.or.
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
     *              ' *****DSS*** zrrts6:  ERROR  - NO VERTICAL DATUM',
     *              ' OFFSET for ',nativeDatum(1:len_trim(nativeDatum)),
     *              ' to ',cvdatum1(1:len_trim(cvdatum1)),
     *              ' Elevations were not converted.'
                  end if
                  istat = 13
                  go to 990
                end if
                !------------------------------------------------------------!
                ! convert the vertical datum offset to the units of the data !
                !------------------------------------------------------------!
                call holchr (iihead(2),  1, 8, cscrat, 1)
                call getoffset(vertDatumOffset, unit, cscrat) !cstrat contains unit
                if (vertDatumOffset.eq.
     *            UNDEFINED_VERTICAL_DATUM_VALUE)then
                  if (mlevel.ge.1) then
                    write (munit,'(/,a,a,a,a,a,a,a,/,a)')
     *              ' *****DSS*** zrrts6:  ERROR  - INVALID DATA UNIT ',
     *              '(',cunits(1:len_trim(cunits)),') OR OFFSET UNIT (',
     *              unit(1:len_trim(unit)),') FOR VERTICAL DATUM',
     *              ' CONVERSION',
     *              ' Elevations were not converted.'
                  end if
                  istat = 13
                  go to 990
                end if
                !-----------------------------------!
                ! add the offset to the data values !
                !-----------------------------------!
                if (lgetdob) then
                  do i = nstart_b4, nstart_b4+nread-1
                    if (dvalues(i).ne.-901.and.dvalues(i).ne.-902) then
                      dvalues(i) = dvalues(i) + vertDatumOffset
                    end if
                  end do
                else
                  do i = nstart_b4, nstart_b4+nread-1
                    if (svalues(i).ne.-901.and.svalues(i).ne.-902) then
                      svalues(i) = svalues(i) + vertDatumOffset
                    end if
                  end do
                end if
              end if
            end if
          end if
        end if
      end if
C
C     Clear the Buffer save flags set above by calling zbdump6
      CALL zbdump6 (IFLTAB, 1)
C
C
C     If this record was not found, write an error message
C
      IF (.NOT.LFOUND) THEN
C
      CALL CHRLNB(CTSPAT,N)
      IF ((ISTAT.EQ.0).AND.(MLEVEL.GE.2)) WRITE (MUNIT,240)IFUNIT,
     * CTSPAT(1:N)
 240  FORMAT (' -----DSS*** zrrts6:  CAUTION - Data block not ',
     * 'found in file.  Unit:',I5,/,' Pathname: ',A)
C
      IF (.NOT.LTIMEW) THEN
      ISTAT = 5
      IOFSET = 0
      call strcpy(CUNITS, ' ')
      call strcpy(CTYPE, ' ')
      IRTZONE = -1
      GO TO 800
      ELSE
      ISTAT = 2
      ENDIF
C
      ELSE
C     Get the data time offset, data type, and data units
      IOFSET = IIHEAD(1)
      CALL HOLCHR (IIHEAD(2),  1, 8, CSCRAT, 1)
      call strcpy(CUNITS, CSCRAT)
      CALL HOLCHR (IIHEAD(4), 1, 8, CSCRAT,  1)
      call strcpy(CTYPE, CSCRAT)
      LPATH = .TRUE.
      IF (NIHEAD.GE.6) THEN
         IRTZONE = IIHEAD(6)
      ELSE
         IRTZONE = -1
      ENDIF
      call strcpy(CRTZONE, ' ')
	LCOORDS = .FALSE.
      IF (NIHEAD.EQ.9) THEN
         CALL HOLCHR (IIHEAD(7), 1, 12, CRTZONE,  1)
      ELSE IF (NIHEAD.GE.12) THEN
         CALL HOLCHR (IIHEAD(7), 1, 24, CRTZONE,  1)
	   IF (NIHEAD.GE.24) THEN
	      CALL zmovwd6(IIHEAD(13), COORDS(1), 6)
            DO 250 I=1,6
               ICDESC(I) = IIHEAD(I+18)
 250        CONTINUE
            DO 260 I=1,3
	         IF (COORDS(I).NE.0.0) LCOORDS = .TRUE.
 260        CONTINUE
         ENDIF
      ENDIF
      ENDIF
C
      IF ((LQBLOK).AND.(LQUAL)) LQREAD = .TRUE.
C
C     If not time window set, set the status flag and return.
      IF (LTIMEW) THEN
C     Are we done reading the data?
      IF (NSTART.GT.KVALS) GO TO 300
      IF (JUL.GT.JULAST) GO TO 300
C     Need to read more data, loop back to 100
      GO TO 100
      ENDIF
C
C
C     Reading complete.  Locate any missing data and indicate in ISTAT
 300  CONTINUE
      IF (ISTAT.GE.4) GO TO 800
      LFOUND = .FALSE.
      IF (.NOT.LGETDOB) THEN
      DO 320 I=1,NVALS
      IF (SVALUES(I).EQ.-901.0) THEN
      IF (ISTAT.LT.1) THEN
      ISTAT = 1
      ELSE IF (ISTAT.GT.1) THEN
      ISTAT = 3
      ENDIF
      ELSE IF (SVALUES(I).EQ.-902.0) THEN
      ELSE
      LFOUND = .TRUE.
      ENDIF
 320  CONTINUE
      ELSE
C
      DO 340 I=1,NVALS
      IF (DVALUES(I).EQ.-901.0) THEN
      IF (ISTAT.LT.1) THEN
      ISTAT = 1
      ELSE IF (ISTAT.GT.1) THEN
      ISTAT = 3
      ENDIF
      ELSE IF (DVALUES(I).EQ.-902.0) THEN
      ELSE
      LFOUND = .TRUE.
      ENDIF
 340  CONTINUE
      ENDIF
C
C     All missing data, but pathname(s) read?
      IF (.NOT.LFOUND) ISTAT = 4
C     No Pathnames found?
      IF (.NOT.LPATH)  THEN
          ISTAT = 5
          IOFSET = 0
          call strcpy(CUNITS, ' ')
          call strcpy(CTYPE, ' ')
      ENDIF
C
C
 800  CONTINUE
      IF (ISTAT.GE.5) THEN
      call strcpy(CUNITS, ' ')
      call strcpy(CTYPE, ' ')
      ENDIF
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, ISTAT, IOFSET,
     * CUNITS, CTYPE
 820  FORMAT(T8,'----- Exiting zrrts6, Number of data values:',I7,
     * ',  Status:',I3/,T10,'Offset:',I8,',  Units: ',A,',  Type:',A)
*      CALL FLUSH(MUNIT)
C
      RETURN
C
C
C     --- ERROR STATEMENTS ---
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,901) NPATH, CPATH(1:NPATH)
 901  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - ILLEGAL PATHNAME',
     * ' OR PATHAME LENGTH',/,' Length: ',I5,/,' Pathname: ',A,/)
      ISTAT = 24
      GO TO 990
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,911) CPATH(IBPART(5):IEPART(5)),
     * INTL, CPATH
 911  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - NON-STANDARD TIME',
     * ' INTERVAL',/,' Interval: ',A,2X,I8,/,' Pathname: ',A,/)
      ISTAT = 12
      GO TO 990
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,921) NVALS, CPATH(1:NPATH)
 921  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - NUMBER OF VALUES',
     * ' REQUESTED LESS THAN 1',/,' NVALS: ',I8,/,' Pathname: ',A,/)
      ISTAT = 11
      GO TO 990
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,931) CRDATE, JULS, CPATH(1:NPATH)
 931  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - ILLEGAL STARTING',
     * ' DATE SPECIFIED',/,' Date: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 940  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,941) CRTIME, ISTIME, CPATH(1:NPATH)
 941  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - ILLEGAL STARTING',
     * ' TIME SPECIFIED',/,' Time: ',A,3X,I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 950  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,951) CDATE1, IYR, IMON, IDAY,
     * CPATH(1:NPATH)
 951  FORMAT(/,' *****DSS*** zrrts6:  ERROR  - UNABLE TO GENERATE',
     * ' BLOCK DATE',/,' Date: ',A,3X,3I8,/,' Pathname: ',A,/)
      ISTAT = 15
      GO TO 990
C
 960  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,961) CPATH(1:NPATH)
 961  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - NO TIME',
     * ' INTERVAL',/,' Pathname: ',A,/)
      ISTAT = 12
      GO TO 990
C
 970  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,971) CPATH(1:NPATH), IFLTAB(KDTYPE)
 971  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - The Data is not',
     * ' Regular-Interval Time Series',/,' Pathname: ',A,/,
     * ' Data Type:',I5,/)
      ISTAT = 20
      GO TO 990
C
 979  CONTINUE
      ISTAT = IFLTAB(KSTAT)
 980  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT,981) ISTAT, CPATH(1:NPATH)
 981  FORMAT (/,' *****DSS*** zrrts6:  ERROR  - UNABLE TO ',
     * ' RETRIEVE DATA',/,' Status: ',I8,/,' Pathname: ',A,/)
      GO TO 990
C
C
 990  CONTINUE
      NVALS = 0
      IF (MLEVEL.GE.7) WRITE (MUNIT,991) ISTAT
 991  FORMAT(T10,'----- Exiting zrrts6, Error return; Status:',I4)
      CALL FLUSH(MUNIT)                                                 Mu
C
      RETURN
      END

