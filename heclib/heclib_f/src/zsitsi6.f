      SUBROUTINE zsitsi6(IFLTAB, CPATHNAME, ITIMES, SVALUES, DVALUES,
     * LDOUBLE, NVALUE, IBDATE, JQUAL, LSQUAL, CUNITS, CTYPE,
     * IUHEAD, NUHEAD, COORDS, NCOORDS, ICDESC, NCDESC,INFLAG, ISTAT)
C
C
C     Z - Store - IRregular - Time - Series  data into DSS database
C     (Extended version)
C
C     Written by Bill Charley
C
C
C     --- Arguments ---
C     Input:
C         IFLTAB -  File table array used in zopen6 CALL (DIM to 1200)
C         CPATH  - Pathname
C        SVALUES - The data values to be stored by the subroutine.
C         ITIMES - An array containing the minutes of each data value,
C                  from the base date, IBDATE
C         NVALS -  Number if date/value pairs to store.
C         IBDATE -  Base date - real julian date of starting time; add
C                  ITIMES(I) to this to get the date for (I).
C         CUNITS - Units of data, CHARACTER*8
C         CTYPE -  Type of data, CHARACTER*8
C         INFLAG - Flag to indicate whether to replace or merge new data
C                  with old data. Replace for editing/changing data,
C                  merge for adding data.
C                  = 0  FOR MERGE
C                  = 1  FOR REPLACE
C
C         ISTAT -  Status parameter on operation
C                  = 0 IF OK
C                  = 4 IF NO DATA FOUND
C                  .GE. 10 IF A 'FATAL' ERROR OCCURED
C
C     Data is stored in a buffer array in the following fashion
C     time1, value1, quality1, time2, value2, quality2
C
C     value may be single or double
C     quality may or may not be stored
C
C
      INTEGER IFLTAB(*), JQUAL(*), ITIMES(*), IUHEAD(*)
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*), COORDS(*)
      INTEGER NCDESC, NCOORDS, ICDESC(*)
      LOGICAL LDOUBLE
C
      INTEGER IBPART(6), IEPART(6), ILPART(6), NPATH, INTLPS
      CHARACTER CPATHNAME*(*), CUNITS*(*), CTYPE*(*)
      CHARACTER CPATH*392, CPATH1*392, CDATE1*9, CDATE2*9
      CHARACTER CTIME1*4, CTIME2*4, CDATE*9, CSCRAT*20
      LOGICAL LFILDOB
C
      LOGICAL LCOMPAR

      PARAMETER (KIHEAD = 28)
      INTEGER IIHEAD(KIHEAD), IDUM(2)
C
C     Vertical datum varible dimensions
      include 'dss_parameters.h'
      character*400 vdiStr, errMsg
      character*16 unit, unit2, cvdatum1, cvdatum2
      character*16 nativeDatum, prevVerticalDatum
      character*64 unitSpec
      character(len=dss_maxpart) ca, cb, cc, cd, ce, cf
      character(len=dss_maxpath) cpath_this, cpath_next
      double precision offsetNavd88, offsetNgvd29, vertDatumOffset
      logical l_Navd88Estimated, l_Ngvd29Estimated, l_modified
      integer vdiStrLen, nuhead_copy, iuhead_copy(100)
      integer na, nb, nc, nd, ne, nf
C
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'verticalDatumFortran.h'
C
C
      LOGICAL LF, LCLEAR, LQUAL, LSQUAL, LDEL, LDSWAP
C
C
      l_modified = .false.
      nuhead_copy = 0
      ISTAT = 0
      IPOS = 1
      I1440 = 1440
      NVALS = NVALUE
      LDSWAP = .FALSE.
      IF (IFLTAB(KDSWAP).NE.0) LDSWAP = .TRUE.
      CALL zset6('QUAL', 'OFF', 0)
      CPATH = CPATHNAME
      CALL CHRLNB(CPATHNAME, NPATH)
      prevVerticalDatum = 'never set'
      ca = ' '
      cb = ' '
      cc = ' '
      cd = ' '
      ce = ' '
      cf = ' '
      na = dss_maxpart
      nb = dss_maxpart
      nc = dss_maxpart
      nd = dss_maxpart
      ne = dss_maxpart
      nf = dss_maxpart
C
C
C     Get beginning and ending dates from DATE array
      CALL DATCLL (IBDATE, ITIMES(1), JULS, ISTIME)
      CALL DATCLL (IBDATE, ITIMES(NVALS), JULE, IETIME)
C
C
      IF (MLEVEL.GE.9) THEN
      CALL JULDAT (JULS, 114, CDATE1, N)
      CALL JULDAT (JULE, 114, CDATE2, N)
      WRITE (MUNIT, 20) JULS, ISTIME, CDATE1, JULE, IETIME, CDATE2,
     * CPATH(1:NPATH)
 20   FORMAT(T10,'----- ENTERING zsitsi6-----',
     */,T5,'Starting date and time: ',3X,2I8,2X,A,
     */,T5,'Ending date and time:   ',3X,2I8,2X,A,
     */,T5,'Pathname: ',A)
      WRITE (MUNIT,30) KLBUFF,NVALS,INFLAG
 30   FORMAT(T5,'BUFF DIM:',I6,'  NUMBER OF DATA:',I6,',  INFLAG:',I4)
      J = NVALS
      IF (J.GT.25) J = 25
      DO 40 I=1,J
      IF (LDOUBLE) THEN
      WRITE (MUNIT,35) I, ITIMES(I), DVALUES(I)
      ELSE
      WRITE (MUNIT,35) I, ITIMES(I), SVALUES(I)
      ENDIF
 35   FORMAT (' Element',I4,',  Time:',I8,',  Value:',F10.1)
 40   CONTINUE
      ENDIF
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zsitsi6', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
C     Check that the dates are in order
      IF (NVALS.NE.1) THEN
      DO 70 J=1,NVALS-1
      K = J + 1
      IF (ITIMES(K).LT.ITIMES(J)) THEN
      WRITE (MUNIT,60) CPATH(1:NPATH), ITIMES(J), ITIMES(K)
 60   FORMAT(' -----DSS*** zsitsi6:  Error - Times are not Ascending:',
     * /,' Pathname: ',A,/,' Relative Times: ',2I10)
C
C     Figure out dates/times to write out in a nice fashion
      CALL DATCLL (IBDATE, ITIMES(J), IDATE, ITIME)
      CALL JULDAT (IDATE, 104, CDATE1, ND)
      ND = M2IHM (ITIME, CTIME1)
C
      CALL DATCLL (IBDATE, ITIMES(K), IDATE, ITIME)
      CALL JULDAT (IDATE, 104, CDATE2, ND)
      ND = M2IHM (ITIME, CTIME2)
      IF (LDOUBLE) THEN
      WRITE (MUNIT,65) J, DVALUES(J), ITIMES(J), CDATE1, CTIME1,
     * K, DVALUES(K), ITIMES(K), CDATE2, CTIME2
      ELSE
      WRITE (MUNIT,65) J, SVALUES(J), ITIMES(J), CDATE1, CTIME1,
     * K, SVALUES(K), ITIMES(K), CDATE2, CTIME2
      ENDIF
 65   FORMAT (2(' Element',I4,', Value:',F8.3,' Time offset',I6,
     * ',  has a date of ',A,', and time ',A,/))
      ISTAT = 30
      GO TO 800
      ENDIF
 70   CONTINUE
      ENDIF
C
C
      IF (CPATH(1:1).NE.'/') GO TO 910
C     Look for a PSEUDO regular time series pathname... The e part
C     starts with a tilde.  We treat this fully as an irregular
C     interval data set, but the user sees the general interval
      I = INDEX(CPATH(1:NPATH), '/~')
      IF (I.GT.2) THEN
        CALL ZPseudoRTS6(CPATHNAME(1:NPATH), CPATH, INTLPS, 1, ISTAT)
        IF (ISTAT.EQ.0) THEN
          CALL CHRLNB(CPATH, NPATH)
        ELSE
          INTLPS = 0
        ENDIF
      ELSE
        INTLPS = 0
      ENDIF
      CALL ZUPATH (CPATH(1:NPATH), IBPART, IEPART, ILPART, ISTAT)
      IF (ISTAT.NE.0) GO TO 910
C
      CALL zirbeg6 (IFLTAB, JULS, CPATH(IBPART(5):IEPART(5)),
     * IYR, IMON, IDAY, IBLOCK, MINBLK, INCBLK)
      IF (IBLOCK.LE.0) GO TO 910
C
C
C     Check for 'Pattern' Time Series, data that
C     has no specific time associated with it
C     (such as a unit hydrograph or mean maximum daily temperatures)
C
        IF (CPATH(IBPART(4):IBPART(4)+2).EQ.'TS-') THEN
C
         JULSB = IYMDJL (IYR, IMON, IDAY)
         IDIFF = (JULSB - IBDATE) * 1440
C
         IPOS = 1
         DO 80 I=1,NVALUE
            N = ((I-1)*2) + 1
            IF (N+1.GE.KLBUFF) GO TO 900
            ILBUFF(N) = ITIMES(IPOS) - IDIFF
            NDA = N+1
            BUFF(NDA) = SVALUES(IPOS)
           IPOS = IPOS + 1
 80      CONTINUE
C
C        Move header information into arguments
         IIHEAD(1) = NVALUE
         IIHEAD(2) = NVALUE
         IIHEAD(3) = JULSB
C        Relative time of last data value
         IIHEAD(4) = ILBUFF((NVALUE*2)-1)
         IIHEAD(5) = INTLPS
         CSCRAT = CUNITS
         CALL CHRHOL (CSCRAT, 1, 8, IIHEAD(6),  1)
         CSCRAT = CTYPE
         CALL CHRHOL (CSCRAT, 1, 8, IIHEAD(8), 1)
         IIHEAD(10) = IWTZONE
         CALL CHRHOL (CWTZONE, 1, 24, IIHEAD(11), 1)
C        Add coordinate info
         DO 90 I=17,28
            IIHEAD(I) = 0
 90	   CONTINUE
         IF (NCOORDS.GT.0) THEN
         CALL zmovwd6(COORDS(1), IIHEAD(17), NCOORDS*2)
         NDES = MIN(NCDESC, 6)
         DO 95 I=1,NDES
            IIHEAD(I+22) = ICDESC(I)
 95      CONTINUE
         ENDIF
C        NIHEAD = 28
C
         CALL zset6 ('TYPE', ' ',110)
         CALL zset6 ('PSEU', ' ',INTLPS)
         CPATH1 = CPATH
         CALL CHRLNB(CPATH1,NPATH)
         CALL zwritex6 (IFLTAB, CPATH1, NPATH, IIHEAD, KIHEAD, IDUM, 0,
     *      IUHEAD, NUHEAD, BUFF, NDA, 111, 0, IST, LF)
         CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
         IF (JSTAT.NE.0) GO TO 940
          CALL zsetfi (IFLTAB, 'PSEU', CPATH1(1:NPATH), INTLPS, IST)
      ENDIF
      !-----------------------------------------------!
      ! convert to native vertical datum if necessary !
      !-----------------------------------------------!
      cc = cpath(ibpart(3):iepart(3))
      call upcase(cc)
      if (index(cc,'ELEV').eq.1) then
        !-----------------------!
        ! elevation time series !
        !-----------------------!
        !----------------------------------------------------------------!
        ! loop through all the records for the time series, checking VDI !
        ! (juls, jule are starting and ending julians and are set above) !
        !----------------------------------------------------------------!
        call zufpn (ca, na, cb, nb, cc, nc, cd, nd, ce, ne, cf, nf,
     *    cpath, npath, iistat)
        call juldat (juls, 104, cd, maxpart)
        do
          call zpath(ca, cb, cc, cd, ce, cf, cpath_this, npath)
          call zreadx6 (ifltab, cpath_this(1:npath), idum, 0, idum, idum
     *      , 0, idum, iuhead_copy, size(iuhead_copy), nuhead_copy, idum
     *      , 0, idum, 0, lfound)
          if ((lfound.ne.0).and.(nuhead_copy.gt.0)) then
            call get_user_header_param(
     *        iuhead_copy,
     *        nuhead_copy,
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
            if (nativeDatum.ne.prevVerticalDatum) then
              if (mlevel.ge.1) then
                write (munit,'(/,a,/,a)')
     *          ' *****DSS*** zsitsi6:  ERROR  - VERTICAL DATUM'//
     *          ' CONFLICT',
     *          ' Elevation values in file are in multiple native'//
     *          ' vertical datums.',
     *          ' No values stored.'
              end if
              istat = 13
              return
            end if
          end if
          call znextts6 (ifltab, cpath_this, cpath_next, .true., iistat)
          call zufpn (ca, na, cb, nb, cc, nc, cd, nd, ce, ne, cf, nf,
     *      cpath_next, npath, iistat)
          call datjul(cd, ijul, iistat)
          if (ijul.ge.jule) exit
        end do
        !-------------------------------------------------------------------!
        ! compare any VDI found in the file with the VDI in the user header !
        !-------------------------------------------------------------------!
        !--------------------------------------------------------------------!
        ! make a copy of the user header becuase we have to be able to know  !
        ! its size when passing to user header manipulation routintes and we !
        ! cant know the size of an assumed-size array                        !
        !--------------------------------------------------------------------!
        if (nuhead.gt.size(iuhead_copy)) then
          if (mlevel.ge.1) then
            write (munit,'(/,a,/,a,i5,/,a,i5)')
     *      ' *****DSS*** zsitsi6:  WARNING  - USER HEADER TRUNCATED',
     *      ' Origninal size = ', nuhead,
     *      ' Truncated size = ', size(iuhead_copy)
          end if
        end if
        nuhead_copy = min(size(iuhead_copy), nuhead)
        iuhead_copy = 0
        iuhead_copy(:nuhead_copy) = iuhead(:nuhead_copy)
	  if (ifltab(kswap).ne.0) then
	    do i = i, nuhead_copy
	      call zswap6(iuhead_copy(i), iuhead_copy(i))
	    end do
	  end if
        call normalizeVdiInUserHeader(iuhead_copy, nuhead_copy, errMsg)
        call get_user_header_param(iuhead_copy, nuhead_copy,
     *    VERTICAL_DATUM_INFO_PARAM, vdiStr)
        if (vdiStr.eq." ") then
          !--------------------------------!
          ! incoming values don't have VDI !
          !--------------------------------!
          if (prevVerticalDatum.ne.'never set') then
            if (mlevel.ge.1) then
              write (munit,'(/,a,/,a)')
     *        ' *****DSS*** zsitsi6:  ERROR  - VERTICAL DATUM CONFLICT',
     *        ' Elevation values being stored do not specify native'//
     *        ' vertical datum but the values in the file are in '//
     *        prevVerticalDatum(1:len_trim(prevVerticalDatum))//'.',
     *        ' No values stored.'
            end if
            istat = 13
            return
          end if
        else
          !--------------------------!
          ! incoming values have VDI !
          !--------------------------!
          call stringToVerticalDatumInfo(
     *      vdiStr,
     *      errMsg,
     *      nativeDatum,
     *      unit,
     *      offsetNgvd29,
     *      l_Ngvd29Estimated,
     *      offsetNavd88,
     *      l_Navd88Estimated)
          if (errMsg.ne." ") then
            if (mlevel.ge.1) then
              write (munit,'(/,a,a,/,a,/,a)')
     *          ' *****DSS*** zsitsi6:  ERROR  - ',
     *          errMsg(1:len_trim(errMsg)),
     *          ' Cannot convert to native datum.',
     *          ' No values stored.'
            end if
            istat = 13
            return
          end if
          if (prevVerticalDatum.ne.'never set'.and.
     *        prevVerticalDatum.ne.nativeDatum) then
            if (mlevel.ge.1) then
              write (munit,'(/,a,/,a)')
     *        ' *****DSS*** zsitsi6:  ERROR  - VERTICAL DATUM CONFLICT',
     *        ' Elevation values being stored specify native vertical'//
     *        ' datum of '//nativeDatum(1:len_trim(nativeDatum))//
     *        ' but the values in the file are in '//
     *        prevVerticalDatum(1:len_trim(prevVerticalDatum))//'.',
     *        ' No values stored.'
            end if
            istat = 13
            return
          end if
        end if
        !--------------------------------------!
        ! get the vertical datum of the values !
        !--------------------------------------!
        ! first get any default vertical datum
        call zinqir6(IFLTAB, 'VDTM', cvdatum1, ivdatum1)
        ! override the default with any datum in the user header
        call get_user_header_param(iuhead_copy, nuhead_copy,
     *    VERTICAL_DATUM_PARAM, cvdatum2)
        if (cvdatum2.ne." ") then
          cvdatum1 = cvdatum2
          !---------------------------------------------------------------------------!
          ! remove current vertical datum from user header so it is not saved to disk !
          !---------------------------------------------------------------------------!
          call remove_user_header_param(iuhead_copy,
     *    nuhead_copy, size(iuhead_copy), VERTICAL_DATUM_PARAM)
        end if
        ! override both with the unit spec
        call crack_unit_spec(cunits, unit2, cvdatum2)
        if (cvdatum2.ne.' ') then
          cunits = unit2(1:len_trim(unit2))
          cvdatum1 = cvdatum2
        end if
        if (cvdatum1.ne.CVD_UNSET) then
          !--------------------------------------------!
          ! we possibly need to convert the elevations !
          !--------------------------------------------!
          call get_user_header_param(iuhead_copy, nuhead_copy,
     *      VERTICAL_DATUM_INFO_PARAM, vdiStr)
          if (vdiStr.eq." ") then
            if (mlevel.ge.1) then
              write (munit,'(/,a,a,/,a,a,a,/,a)')
     *          ' *****DSS*** zsitsi6:  ERROR  - NO VERTICAL DATUM',
     *          ' OFFSET INFORMATION.',' Cannot convert from ',
     *          cvdatum1(1:len_trim(cvdatum1)),' to native datum.',
     *          ' No values stored.'
            end if
            istat = 13
            return
          else
            call stringToVerticalDatumInfo(
     *        vdiStr,
     *        errMsg,
     *        nativeDatum,
     *        unit,
     *        offsetNgvd29,
     *        l_Ngvd29Estimated,
     *        offsetNavd88,
     *        l_Navd88Estimated)
            if (errMsg.ne.' ') then
              if (mlevel.ge.1) then
                write (munit,'(/,a,a,/,a,/,a)')
     *            ' *****DSS*** zsitsi6:  ERROR  - ',
     *            errMsg(1:len_trim(errMsg)),
     *            ' Cannot convert to native datum.',
     *            ' No values stored.'
              end if
              istat = 13
              return
            end if
            if (cvdatum1.eq.CVD_NAVD88) then
              vertDatumOffset = offsetNavd88
            elseif (cvdatum1.eq.CVD_NGVD29) then
              vertDatumOffset = offsetNgvd29
            else
              if (nativeDatum.eq.cvdatum1.or.
     *            nativeDatum.eq.CVD_OTHER) then
                vertDatumOffset = 0.
              else
                vertDatumOffset = UNDEFINED_VERTICAL_DATUM_VALUE
              end if
            end if
            if (vertDatumOffset.ne.0) then
              if(vertDatumOffset.eq.UNDEFINED_VERTICAL_DATUM_VALUE) then
                if (mlevel.ge.1) then
                  write (munit,'(/,a,a,a,a,a,/,a)')
     *            ' *****DSS*** zsitsi6:  ERROR  - NO VERTICAL DATUM',
     *            ' OFFSET for ',nativeDatum(1:len_trim(nativeDatum)),
     *            ' to ',cvdatum1(1:len_trim(cvdatum1)),
     *            ' No values stored.'
                end if
                istat = 13
                return
              end if
              call getOffset(
     *              vertDatumOffset,
     *              unit(1:len_trim(unit)),
     *              cunits(1:len_trim(cunits)))
              if (vertDatumOffset.eq.
     *          UNDEFINED_VERTICAL_DATUM_VALUE)then
                if (mlevel.ge.1) then
                  write (munit,'(/,a,a,a,a,a,a,a,a,/,a)')
     *            ' *****DSS*** zsitsi6:  ERROR  - INVALID DATA UNIT',
     *            ' (',cunits(1:len_trim(cunits)),') OR OFFSET UNIT',
     *            ' (',unit(1:len_trim(unit)),') FOR VERTICAL DATUM',
     *            ' CONVERSION',
     *            ' No values stored.'
                end if
                istat = 13
                return
              end if
              if (ldouble) then
                do ii = 1, nvals
                  if (dvalues(ii).ne.-901.and.dvalues(ii).ne.-902.) then
                    dvalues(ii) = dvalues(ii) - vertdatumoffset
                  end if
                end do
              else
                do ii = 1, nvals
                  if (svalues(ii).ne.-901.and.svalues(ii).ne.-902.) then
                    svalues(ii) = svalues(ii) - vertdatumoffset
                  end if
                end do
              end if
              l_modified = .true.
            end if
          end if
        end if
      else
        if (nuhead.gt.size(iuhead_copy)) then
          if (mlevel.ge.1) then
            write (munit,'(/,a,/,a,i5,/,a,i5)')
     *      ' *****DSS*** zsrtsi6:  WARNING  - USER HEADER TRUNCATED',
     *      ' Origninal size = ', nuhead,
     *      ' Truncated size = ', size(iuhead_copy)
          end if
        end if
        nuhead_copy = min(size(iuhead_copy), nuhead)
        iuhead_copy = 0
        iuhead_copy(:nuhead_copy) = iuhead(:nuhead_copy)
      end if
      cunits = cunits(:min(len_trim(cunits),8))
C
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
C
C
C     Major loop through time blocks
 100  CONTINUE
C
      JULSB = JUL
      LCLEAR = .FALSE.
      LDEL = .FALSE.
C     Form the pathname
      CALL JULDAT (JUL, 104, CDATE, N)
      CPATH1(IBPART(4):IBPART(4)+8) = CDATE
C
C     Get starting date of subsequent blocks
      CALL zincbk6 (IBLOCK, JUL, IYR, IMON, IDAY)
C
C
C     Get starting and ending dates of existing data block
      JULEB = JUL - 1
C
C
C     If data is to be replaced and times encompass entire
C     block--don't read, just replace block
      IF ((INFLAG.EQ.1).AND.(.NOT.LQPBIT)) THEN
      IF ((JULS.LT.JULSB).AND.(JULE.GE.JUL)) GO TO 500
      END IF
C
C
      CALL zreadx6 (IFLTAB, CPATH1, INTBUF, NIBUFF, N,
     * ICHEAD, 0, J, iuhead_copy, 0, N, BUFF, KLBUFF, NDA, 2, LF)
      CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
      IF (JSTAT.NE.0) GO TO 940
C
      IF (.NOT.LF) GO TO 500
C
C     Check that the buffer size is large enough.
      IF (NDA.GT.KLBUFF) GO TO 920
C
C     Found data
C
      IBSIZE = INTBUF(1)
      NPAIRS = INTBUF(2)
      IF ((IBSIZE.LE.0).OR.(NPAIRS.LE.0)) GO TO 500
      CALL zinqir6 (IFLTAB, 'QUAL', CSCRAT, NQUAL)
      IF (NQUAL.EQ.1) THEN
      LQUAL = .TRUE.
      ELSE
      LQUAL = .FALSE.
      ENDIF
C
      IF (LQUAL.NEQV.LSQUAL) THEN
         CALL CHRLNB(CPATH1, NPATH1)
         IF (LSQUAL) THEN
            IF (MLEVEL.GE.3) WRITE (MUNIT, 110) CPATH1(1:NPATH1)
 110        FORMAT (' -----DSS---zsitsi6: Caution:  Writing flags to an',
     *      ' existing data set that does not have flags.',/,
     *      ' Pathname: ',A)
         ELSE
            IF (LQPBIT) THEN
C              If the protection bit flag is on, we cannot write a
C              record without flags to one that has flags
               IF (MLEVEL.GE.2) WRITE (MUNIT, 115) CPATH1(1:NPATH1)
 115           FORMAT (' -----DSS---zsrtsx6:  Write Protection for',
     *         ' Existing Record (no data written)',/,
     *         ' Cannot write data without flags to an existing',
     *         ' data set that has flags.',/,
     *         ' Pathname: ',A)
               GO TO 620
            ENDIF
            IF (MLEVEL.GE.3) WRITE (MUNIT, 120) CPATH1(1:NPATH1)
 120        FORMAT (' -----DSS---zsitsi6: Caution:  Writing data ',
     *      ' without flags to an existing data set that has flags.',/,
     *      ' Pathname: ',A)
         ENDIF
      ENDIF
C
C     Is the data set single or double?
      CALL zinqir6 (IFLTAB, 'TYPE', CSCRAT, NTYPE)
      IF (NTYPE.EQ.115) THEN
         LFILDOB = .TRUE.
      ELSE
         LFILDOB = .FALSE.
      ENDIF
C
      IF (LDOUBLE.NEQV.LFILDOB) THEN
C        Convert the data set read to match what we will store
         IF (.NOT.LQUAL) THEN
C           No quality flags to take care of
            IF (LDOUBLE) THEN
               IF (NPAIRS*3.GT.KLBUFF) GO TO 900
               DO 150 I=NPAIRS,1,-1
                  CALL zirdbl6 (ILBUFF(I*2), ILBUFF(I*3-1), .TRUE.)
                  ILBUFF(I*3-2) = ILBUFF(I*2-1)
 150           CONTINUE
            ELSE
C              Reduce the array size
               DO 160 I=1,NPAIRS
                  ILBUFF(I*2-1) = ILBUFF(I*3-2)
                  CALL zirdbl6 (ILBUFF(I*3-1), ILBUFF(I*2), .FALSE.)
 160           CONTINUE
            ENDIF
         ELSE
C           We must account for quality flags
            IF (LDOUBLE) THEN
               IF (NPAIRS*4.GT.KLBUFF) GO TO 900
               DO 170 I=NPAIRS,1,-1
                  ILBUFF(I*4) = ILBUFF(I*3)
                  CALL zirdbl6 (ILBUFF(I*3-1), ILBUFF(I*4-2), .TRUE.)
                  ILBUFF(I*4-3) = ILBUFF(I*3-2)
 170           CONTINUE
            ELSE
               DO 180 I=1,NPAIRS
                  ILBUFF(I*3-2) = ILBUFF(I*4-3)
                  CALL zirdbl6 (ILBUFF(I*4-2), ILBUFF(I*3-1), .FALSE.)
                  ILBUFF(I*3) = ILBUFF(I*4)
 180           CONTINUE
            ENDIF
         ENDIF
      ENDIF
C
C
      IF ((LSQUAL).AND.(.NOT.LQUAL)) THEN
C        We are storing quality, but it was not read
C        from the file.  Make space for it
         IF (.NOT.LDOUBLE) THEN
            IF (NPAIRS*3.GT.KLBUFF) GO TO 900
            DO 190 I=NPAIRS,1,-1
               ILBUFF(I*3) = 0
               ILBUFF(I*3-1) = ILBUFF(I*2)
               ILBUFF(I*3-2) = ILBUFF(I*2-1)
 190        CONTINUE
         ELSE
            IF (NPAIRS*4.GT.KLBUFF) GO TO 900
            DO 195 I=NPAIRS,1,-1
               ILBUFF(I*4) = 0
               ILBUFF(I*4-1) = ILBUFF(I*3)
               ILBUFF(I*4-2) = ILBUFF(I*3-1)
               ILBUFF(I*4-3) = ILBUFF(I*3-2)
 195        CONTINUE
         ENDIF
         LQUAL = .TRUE.
      ENDIF
C
      IF (LQUAL) THEN
      IMULT = 3
      ELSE
      IMULT = 2
      ENDIF
C
      IF (LFILDOB) THEN
         IMULT = IMULT + 1
      ENDIF
C
C
C     ----- REPLACE DATA -----
C
      IF (INFLAG.EQ.1) THEN
C
C     There are 6 possible cases for replacing data:
C
C    Block read          Block to be stored
C
C                                                     ____
C                                                    |    |
C                                             ____   | 6  |
C                                     ____   |    |  |    |
C                                    |    |  | 5  |  |____|
C    ______  ........................|....|..|....|............
C   |      |                         |    |  |    |
C   |      |                  ____   |    |  |____|
C   |      |                 |    |  | 4  |
C   | Read |                 | 3  |  |    |
C   |      |          ____   |    |  |    |
C   |      |         |    |  |____|  |    |
C   |______| ........|....|..........|....|....................
C             ____   | 2  |          |    |
C            |    |  |    |          |____|
C            | 1  |  |    |
C            |    |  |____|
C            |____|
C
C
C
C
C     Locate dates in block read
C
      IF (MLEVEL.GE.9) WRITE(MUNIT,202) NPAIRS,IBSIZE
202   FORMAT(' RECORD READ; NPAIRS, IBSIZE = ',2I8)
C
C     Check to see if data starts beford this block
      IF (IPOS.GT.1) THEN
      IBEG = 1
      GO TO 250
      ENDIF
C
C
C     Is this a case #1?
      N = ((NPAIRS-1)*IMULT) + 1
      JTIME = ILBUFF(N)
C     ###### CASE 1 ######
C     Is the current time > than the last value read?
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).GT.0) THEN
C     Data occurs after last piece of data read
C     Add data to the end of BUFF
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT Statement 215'
 215  CONTINUE
C
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
C
C     Check to see if first or last data to be removed (-902.)
      IF (LCOMPAR(SVALUES, DVALUES, IPOS, LDOUBLE, -902.)) THEN
         IF ((IPOS.EQ.1).OR.(IPOS.EQ.NVALS)) GO TO 220
      ENDIF
C
      NPAIRS = NPAIRS + 1
      N = ((NPAIRS-1)*IMULT) + 1
      IF ((N+1).GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      JTIME = ITIMES(IPOS) - (I6 * I1440)
      CALL zircpy6 (ILBUFF(N), JTIME, DVALUES, SVALUES,
     *             JQUAL, IPOS, LDOUBLE, LSQUAL, LQUAL, .TRUE., LDSWAP)
C
 220  IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) GO TO 600
      GO TO 215
      ENDIF
C     ######  ######
C
C
C     Locate beginning position of dates() in BUFF
      DO 210 J=1,NPAIRS
      IBEG = J
      N = ((J-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).LE.0) GO TO 250
 210  CONTINUE
C
C     Data beings prior to or in the middle of BUFF
C     Case 2, 3, or 4
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT Statement 250'
 250  CONTINUE
C     Is the last value to be stored .ge. the last value read?
      N = ((NPAIRS-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(NVALS), JULSB, JTIME ).GE.0) THEN
C
C     Times encompass entire block read (Case 4)
      IF (IBEG.EQ.1) GO TO 500
C
C
C     Data is to be replaced in later part of
C     block read  through the end of BUFF)
C
C     ###### CASE 2 ######
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT DO 290'
C     Insert new data into BUFF
C     Is the first value just a flag for the beg of the time window?
      IF (IPOS.EQ.1) THEN
         IF (LCOMPAR(SVALUES, DVALUES, IPOS, LDOUBLE, -902.)) THEN
            IPOS = IPOS + 1
            NPAIRS = IBEG - 1
         ENDIF
      ENDIF
      JPOS = IPOS
      DO 290 I=JPOS,NVALS
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
      NPAIRS = IBEG
      N = ((IBEG-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      JTIME = ITIMES(IPOS) - (I6 * I1440)
      CALL zircpy6 (ILBUFF(N), JTIME, DVALUES, SVALUES,
     *             JQUAL, IPOS, LDOUBLE, LSQUAL, LQUAL, .TRUE., LDSWAP)
      IBEG = IBEG + 1
      IPOS = IPOS + 1
 290  CONTINUE
C
C     Is the last value just a flag for the end of the time window?
      IF (LCOMPAR(SVALUES, DVALUES, NVALS, LDOUBLE, -902.))
     *   NPAIRS = NPAIRS - 1
      GO TO 600
C
C     ######  ######
C
      ELSE
C     Data occurs in the middle of BUFF (i.e., does
C     not go beyond the end of BUFF)
C
C     Obtain the location (in BUFF) of the last piece of data
      DO 300 J=IBEG,NPAIRS
      IEND = J
      N = ((J-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(NVALS), JULSB, JTIME).LT.0) GO TO 310
 300  CONTINUE
C
C     IMPOSSIBLE?????
C
C
C     ##### CASE 3, 5, 6 ######
 310  CONTINUE
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT Statement 310'
C
C     Increase or decrease space in BUFF to allow for new data
      J = NVALS - IPOS + 1
      K = IEND - IBEG
      NDOWN = J - K
      IF (IPOS.EQ.1) THEN
          IF (LCOMPAR(SVALUES, DVALUES, IPOS, LDOUBLE, -902.))
     *       NDOWN = NDOWN - 1
      ENDIF
      IF (NVALS.GT.0) THEN
          IF (LCOMPAR(SVALUES, DVALUES, NVALS, LDOUBLE, -902.))
     *       NDOWN = NDOWN - 1
      ENDIF
      IF (NDOWN.LT.0) LCLEAR = .TRUE.
      NPAIRS = NPAIRS + NDOWN
      IF (NPAIRS*IMULT.GT.KLBUFF) GO TO 900
C
      CALL zirdow6 (IBEG, NPAIRS, NDOWN, IMULT, ILBUFF)
C
C     Insert new data into BUFF
      IEND = IEND + NDOWN - 1
C     Should we delete the first data value (set to -902.)?
      IF (IPOS.EQ.1) THEN
         IF (LCOMPAR(SVALUES, DVALUES, IPOS, LDOUBLE, -902.))
     *      IPOS = IPOS + 1
      ENDIF
      IF (IEND.EQ.NVALS) THEN
         IF (LCOMPAR(SVALUES, DVALUES, NVALS, LDOUBLE, -902.))
     *      IEND = IEND - 1
      ENDIF
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT DO 320'
      DO 320 K=IBEG,IEND
      N = ((K-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      JTIME = ITIMES(IPOS) - (I6 * I1440)
      CALL zircpy6 (ILBUFF(N), JTIME, DVALUES, SVALUES,
     *             JQUAL, IPOS, LDOUBLE, LSQUAL, LQUAL, .TRUE., LDSWAP)
      IPOS = IPOS + 1
 320  CONTINUE
C
      GO TO 600
C
C     #####  ######
C
      ENDIF
C
C
      ELSE
C
C
C     ----- MERGE DATA -----
C
C
      IBEG = 1
C
C     If we are only adding data to end, speed up by not searching BUFF
      N = ((NPAIRS-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).GT.0) GO TO 410
C
C     Loop through block read, replacing and inserting data
 400  CONTINUE
C
C     If date is GREATER than current BUFF date:
C     Loop back and compare next BUFF date)
      N = ((IBEG-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).GT.0) THEN
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
      IBEG = IBEG + 1
      IF (IBEG.GT.NPAIRS) GO TO 410
      GO TO 400
      ENDIF
C
C     If date is LESS than current BUFF date:
C     Move dates and values in BUFF down one (after this spot)
C     to allow for the new piece of data to be added here
      N = ((IBEG-1)*IMULT) + 1
      JTIME = ILBUFF(N)
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULSB, JTIME).LT.0) THEN
      NPAIRS = NPAIRS + 1
      IF (NPAIRS*IMULT.GT.KLBUFF) GO TO 900
      CALL zirdow6 (IBEG, NPAIRS, 1, IMULT, ILBUFF)
      ENDIF
C
C     Insert new date and value here (or replace if equal)
      N = ((IBEG-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      JTIME = ITIMES(IPOS) - (I6 * I1440)
      CALL zircpy6 (ILBUFF(N), JTIME, DVALUES, SVALUES,
     *             JQUAL, IPOS, LDOUBLE, LSQUAL, LQUAL, .TRUE., LDSWAP)
C
      IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) GO TO 600
C
      GO TO 400
C
C
C     The remainder of the data occurs at the end of the block
C     (or into the next block)
 410  CONTINUE
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) GO TO 600
C
      NPAIRS = NPAIRS + 1
      N = ((NPAIRS-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      JTIME = ITIMES(IPOS) - (I6 * I1440)
      CALL zircpy6 (ILBUFF(N), JTIME, DVALUES, SVALUES,
     *             JQUAL, IPOS, LDOUBLE, LSQUAL, LQUAL, .TRUE., LDSWAP)
C
      IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) GO TO 600
      GO TO 410
C
C
      ENDIF
C
C
C
C
C     Block does not exist (or data to be replaced fully encompasses
C     block just read) -- create entirely new block to write
C     ###### CASE 4 ######
 500  CONTINUE
C
      LQUAL = LSQUAL
      IF (LQUAL) THEN
      IMULT = 3
      ELSE
      IMULT = 2
      ENDIF
C
      IF (LDOUBLE) THEN
         IMULT = IMULT + 1
      ENDIF
C
      LCLEAR = .TRUE.
C     Check to see if first or last data to be removed (-902.)
      IF (IPOS.EQ.1) THEN
          IF (LCOMPAR(SVALUES, DVALUES, IPOS, LDOUBLE, -902.))
     *        IPOS = IPOS + 1
      ENDIF
      IF (IPOS.GT.NVALS) GO TO 800
C
C     Look for ending position of data
      IF (MLEVEL.GE.9) WRITE (MUNIT,*)'AT DO 510'
      DO 510 I=1,NVALS
C
      IF (ICTIME (IBDATE, ITIMES(IPOS), JULEB, I1440).GT.0) THEN
      NPAIRS = I - 1
      IF ((NPAIRS.LE.0).AND.(INFLAG.EQ.1)) THEN
C     Are we replacing data, and the new data does not have any
C     values within this block?  If so, delete the block.
      CALL zcheck6 (IFLTAB, CPATH1, NPATH, N, J, LF)
      CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
      IF (JSTAT.NE.0) GO TO 940
      IF (LF) CALL zdelet6 (IFLTAB, CPATH1, NPATH, LF)
      LDEL = .TRUE.
      ENDIF
      GO TO 600
      ENDIF
C
      N = ((I-1)*IMULT) + 1
      IF (N+1.GE.KLBUFF) GO TO 900
      I6 = JULSB - IBDATE
      JTIME = ITIMES(IPOS) - (I6 * I1440)
      CALL zircpy6 (ILBUFF(N), JTIME, DVALUES, SVALUES,
     *             JQUAL, IPOS, LDOUBLE, LSQUAL, LQUAL, .TRUE., LDSWAP)
C
      IPOS = IPOS + 1
      IF (IPOS.GT.NVALS) THEN
      NPAIRS = I
      IF (LCOMPAR(SVALUES, DVALUES, NVALS, LDOUBLE, -902.))
     *    NPAIRS = NPAIRS - 1
      GO TO 600
      ENDIF
 510  CONTINUE
C
      NPAIRS = NVALS
C
 600  CONTINUE
C
C     Is there any data to this block?
      IF ((NPAIRS.LE.0).AND.((LDEL).OR.(INFLAG.NE.1))) GO TO 620
C
C     Determine block size of this record
      JBSIZE = IBSIZE
      IF (NPAIRS.LE.MINBLK) THEN
      IBSIZE = MINBLK
      ELSE
      K = (NPAIRS - MINBLK) / INCBLK
      IBSIZE = MINBLK + ((K+1) * INCBLK)
      ENDIF
      IF (JBSIZE.NE.IBSIZE) LCLEAR = .TRUE.
C
C
      IF (MLEVEL.GE.9) WRITE(MUNIT,540)NPAIRS,IBSIZE
 540  FORMAT(T5,'NUMBER OF PAIRS THIS RECORD = ',I5,' BLOCK SIZE =',I5)
C
C     Clear out 'null data' area (between NPAIRS and IBSIZE)
      IF (LCLEAR) THEN
      NDA = IBSIZE * IMULT
      K = (NPAIRS * IMULT) + 1
      DO 550 I=K,NDA
      ILBUFF(I) = 0
 550  CONTINUE
      ENDIF
C
C
C     Move header information into arguments
      IIHEAD(1) = IBSIZE
      IIHEAD(2) = NPAIRS
      IIHEAD(3) = JULSB
C     Relative time of last data value
      IIHEAD(4) = ILBUFF((NPAIRS*IMULT)-1)
      IIHEAD(5) = INTLPS
      CSCRAT = CUNITS
      CALL CHRHOL (CSCRAT, 1, 8, IIHEAD(6),  1)
      CSCRAT = CTYPE
      CALL CHRHOL (CSCRAT, 1, 8, IIHEAD(8), 1)
      IIHEAD(10) = IWTZONE
      CALL CHRHOL (CWTZONE, 1, 24, IIHEAD(11), 1)
C     Add coordinate info
      DO 560 I=17,28
         IIHEAD(I) = 0
 560	CONTINUE
      IF (NCOORDS.GT.0) THEN
         CALL zmovwd6(COORDS(1), IIHEAD(17), NCOORDS*2)
         NDES = MIN(NCDESC, 6)
         DO 565 I=1,NDES
            IIHEAD(I+22) = ICDESC(I)
 565     CONTINUE
      ENDIF
C     NIHEAD = 28
C
C
C     Write data to DSS
      IF (LDOUBLE) THEN
          ITYPE = 115
      ELSE
          ITYPE = 110
      ENDIF
      CALL zset6 ('TYPE', ' ',ITYPE)
      CALL zset6 ('PSEU', ' ',INTLPS)
      NLDATA = NPAIRS
      IF (LQUAL) CALL zset6 ('QUAL', 'ON', 1)
      NDA = IBSIZE * IMULT
      CALL zwritex6 (IFLTAB, CPATH1, NPATH, IIHEAD, KIHEAD, IDUM, 0,
     * iuhead_copy, nuhead_copy, BUFF, NDA, ITYPE, 0, IST, LF)
      CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
      IF (JSTAT.NE.0) GO TO 940
      CALL zsetfi6(IFLTAB, 'PSEU', CPATH1(1:NPATH), INTLPS, IST)
C
 620  CONTINUE
C     Have we reached the end time yet?
C     IF (IPOS.GT.NVALS) GO TO 800
      IF (JULE.LT.JUL) GO TO 800
C
C     Go read next record
      GO TO 100
C
C
C
C     Finished
 800  CONTINUE
      !-----------------------------------------!
      ! restore the original values in the call !
      !-----------------------------------------!
      if (l_modified) then
        if (ldouble) then
          do ii = 1, nvals
            if (dvalues(ii).ne.-901.and.dvalues(ii).ne.-902.) then
              dvalues(ii) = dvalues(ii) + vertdatumoffset
            end if
          end do
        else
          do ii = 1, nvals
            if (svalues(ii).ne.-901.and.svalues(ii).ne.-902.) then
              svalues(ii) = svalues(ii) + vertdatumoffset
            end if
          end do
        end if
      end if
      IF ((NVALS.EQ.0).AND.(ISTAT.EQ.0)) ISTAT = 4
      IF (MLEVEL.GE.9) WRITE (MUNIT, 820) NVALS, ISTAT
 820  FORMAT (T5,'----- Exiting zsitsi6; Values Written:',I5,
     * ',  Status:',I4)
      IWTZONE = -1
      CWTZONE  = ' '
      RETURN
C
C
C     Error conditions
C     Not enough buffer space to write record to DSS
 900  CONTINUE
      CALL CHRLNB(CPATH1,N)
      WRITE (MUNIT,901) CPATH1(1:N), KLBUFF
 901  FORMAT(/,' ***** ERROR - zsitsi6;  Buffer size not large enough',
     * ' to store this amount of data *****',/,' Pathname: ',A,/,
     * ' Buffer Size:',I6,/)
      ISTAT = 21
      GO TO 800
C
 910  CONTINUE
      CALL CHRLNB(CPATH,N)
      WRITE (MUNIT,911) CPATH(1:N)
 911  FORMAT (' -----DSS*** zsitsi6:  Error - Unable to ',
     *' Recognize Pathname as Irregular Time-Series',/,
     *' Pathname: ',A)
      ISTAT = 24
      GO TO 800
C
 920  CONTINUE
      CALL CHRLNB(CPATH1,N)
      WRITE (MUNIT,921) CPATH1(1:N), NDA, KLBUFF
 921  FORMAT (/,' ***** ERROR - zsitsi6;  The Buffer Array is not',
     *' Large Enough to Read Record',/,
     *' Record: ',A,/' Buffer Size Required:',I6,'  Size Provided',I6/)
      ISTAT = 21
      GO TO 800
C
 940  CONTINUE
      CALL zinqir6 (IFLTAB, 'STATUS', CSCRAT, JSTAT)
      ISTAT = JSTAT
      CALL CHRLNB(CPATH,N)
      IF (MLEVEL.GE.1) WRITE (MUNIT,941) ISTAT, CPATH(1:N)
 941  FORMAT (/,' *****DSS*** zsitsi6:  ERROR  - UNABLE TO ',
     * ' STORE DATA',/,' Status: ',I8,/,' Pathname: ',A,/)
      RETURN
C
C
      END

