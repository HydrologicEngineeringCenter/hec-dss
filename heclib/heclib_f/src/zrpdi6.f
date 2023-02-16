      SUBROUTINE zrpdi6 (IFLTAB, CPATH, NORD, NCURVE, IHORIZ,
     * C1UNIT, C1TYPE, C2UNIT, C2TYPE, SVALUES, DVALUES,
     * LDOUBLE, KVALS, NVALS, CLABEL, KLABEL, LABEL,
     * IUHEAD, KUHEAD, NUHEAD, ISTAT)
C
C
C     Retrieve Paired Data
C
C     Written by Bill Charley at HEC, 1989
C
C
      INTEGER IFLTAB(*), IUHEAD(*)
      CHARACTER CPATH*(*), CLABEL(*)*(*)
      CHARACTER C1UNIT*(*), C1TYPE*(*), C2UNIT*(*), C2TYPE*(*)
      CHARACTER CTEMP*12
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*)
	    INTEGER NH, N, NWDS, ICOUNT
      LOGICAL LFOUND, LABEL, LDOUBLE, LEND
      LOGICAL LFILDOB
C
C     Vertical datum varible dimensions
      integer iuhead_copy(100), paramIsElev
      character*400 vdiStr, errMsg
      character*16 unit
      character*16 nativeDatum, cvdatum1
      double precision offsetNavd88, offsetNgvd29, vertDatumOffset
      logical l_Navd88Estimated, l_Ngvd29Estimated
      logical l_indElev, l_depElev
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdssbf.h'
C
      INCLUDE 'zdssts.h'
C
      INCLUDE 'zdsscc.h'
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'verticalDatumFortran.h'
C
C
C     If debug is on, print out information
      IF (MLEVEL.GE.7) THEN
      CALL CHRLNB(CPATH,N)
      WRITE (MUNIT,20) CPATH(1:N), KVALS, KLABEL, KUHEAD
 20   FORMAT (T5,'----- Enter zrpd6  -----',/,
     * T11,'Pathname: ',A,/,
     * T11,'KVALS:',I7,',  KLABEL:',I5,',  KUHEAD:',I6)
      ENDIF
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6 (IFLTAB, 5, 'zrpd6  ', 0,
     * IFLTAB, ' ', 0, ' ',0)
C
C
      CALL zdtype6 (IFLTAB, CPATH, NDATA, LFOUND, CTEMP, JTYPE)
      CALL zinqir6 (IFLTAB, 'STATUS', CTEMP, JSTAT)
      ISTAT = JSTAT
      IF (ISTAT.NE.0) GO TO 900
      IF (.NOT.LFOUND) THEN
          ISTAT = -1
          GO TO 900
      ENDIF
C
      IF (JTYPE.EQ.200) THEN
         LFILDOB = .FALSE.
      ELSE IF (JTYPE.EQ.205) THEN
         LFILDOB = .TRUE.
      ELSE
         CALL CHRLNB(CPATH,N)
         IF (MLEVEL.GT.1) WRITE (MUNIT,40) CPATH(1:N), JTYPE
 40      FORMAT (' ----DSS---zrpd6;  ERROR:  Record specified was',
     *    ' not stored as Paired Data',/,' Record: ',A,/,
     *    ' Type:',I5)
         ISTAT = 20
         GO TO 900
      ENDIF
C
C     Get the info block, and read the first portion of the header
      IF (LDOUBLE.AND.LFILDOB) THEN
C        Double to double.
C        We have twice the space of singles
         KV = KVALS * 2
         CALL zreadx6 (IFLTAB, CPATH, IGBUFF, NGBUFF, NIHEAD,
     *   ICHEAD, 0, NCHEAD, IUHEAD, KUHEAD, NUHEAD, DVALUES,
     *   KV, NVALS, 0, LFOUND)
         IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DVALUES, NVALS)
         NVALS = NVALS / 2
         IF (NVALS.GT.KVALS) THEN
            CALL CHRLNB(CPATH,N)
            IF (MLEVEL.GT.1) WRITE (MUNIT,910) CPATH(1:N), NVALS, KVALS
         ENDIF
      ELSE IF ((.NOT.LDOUBLE).AND.(.NOT.LFILDOB)) THEN
C        Single to single.
         CALL zreadx6 (IFLTAB, CPATH, IGBUFF, NGBUFF, NIHEAD,
     *   ICHEAD, 0, NCHEAD, IUHEAD, KUHEAD, NUHEAD, SVALUES,
     *   KVALS, NVALS, 0, LFOUND)
         IF (NVALS.GT.KVALS) THEN
            CALL CHRLNB(CPATH,N)
            IF (MLEVEL.GT.1) WRITE (MUNIT,910) CPATH(1:N), NVALS, KVALS
            ISTAT = -1
            GO TO 900
         ENDIF
      ELSE
C        Need to convert data sets.  Check that it is normal size.
         IF ((NDATA.LE.KLBUFF).OR.(KVALS.LE.KLBUFF)) THEN
            CALL zreadx6 (IFLTAB, CPATH, IGBUFF, NGBUFF, NIHEAD,
     *      ICHEAD, 0, NCHEAD, IUHEAD, KUHEAD, NUHEAD, BUFF,
     *      KLBUFF, NVALS, 0, LFOUND)
            IF (NVALS.GT.KLBUFF) THEN
              CALL CHRLNB(CPATH,N)
              IF (MLEVEL.GT.1) WRITE(MUNIT,910)CPATH(1:N),NVALS,KLBUFF
            ENDIF
            IF (LDOUBLE) THEN
C              Data on disk is single, double requested
               NVALS = MIN0(NVALS, KVALS)
               DO 60 I=1,NVALS
                  DVALUES(I) = DBLE(BUFF(I))
 60            CONTINUE
            ELSE
C              Data on disk is double, single requested
               NVALS = MIN0(NVALS, KVALS)
               IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DBUFF, NVALS)
               DO 70 I=1,NVALS
                  SVALUES(I) = SNGL(DBUFF(I))
 70            CONTINUE
            ENDIF
         ELSE
C           Huge data set, buffer in reads.
C           Read internal header, etc., but not data
            CALL zreadx6 (IFLTAB, CPATH, IGBUFF, NGBUFF, NIHEAD,
     *      ICHEAD, 0, NCHEAD, IUHEAD, KUHEAD, NUHEAD, BUFF,
     *      0, NVALS, 0, LFOUND)
            ICOUNT = 0
 80         CONTINUE
            CALL zrdbuf6 (IFLTAB, CPATH, IUHEAD, KUHEAD, NUHEAD,
     *      BUFF, KLBUFF, NDATA, LEND, 0, LFOUND)
            IF (NDATA.GT.KLBUFF) THEN
              CALL CHRLNB(CPATH,N)
              IF (MLEVEL.GT.1) WRITE(MUNIT,910)CPATH(1:N),NDATA,KLBUFF
            ENDIF
            IF (LDOUBLE) THEN
C              Data on disk is single, double requested
               DO 90 I=1,NDATA
                  ICOUNT = ICOUNT + 1
                  DVALUES(ICOUNT) = DBLE(BUFF(I))
                  IF (ICOUNT.GE.KVALS) GO TO 98
 90            CONTINUE
            ELSE
C              Data on disk is double, single requested
               IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DVALUES, NDATA)
               NDATA = NDATA / 2
               DO 95 I=1,NDATA
                  ICOUNT = ICOUNT + 1
                  SVALUES(ICOUNT) = SNGL(DBUFF(I))
                  IF (ICOUNT.GE.KVALS) GO TO 98
 95            CONTINUE
            ENDIF
            IF (.NOT.LEND) GO TO 80
 98         CONTINUE
            NVALS = ICOUNT
         ENDIF
      ENDIF
C
C
      NUHEAD = MIN0 (NUHEAD,KUHEAD)
      NIHEAD = MIN0 (NIHEAD,NGBUFF)
      NORD = IGBUFF(1)
      NCURVE = IGBUFF(2)
      IHORIZ = IGBUFF(3)
C
C     Check that there was enough space in the data array to retrieve
C     all the data specified
      N = (NCURVE + 1) * NORD
      IF (N.GT.KVALS) ISTAT = 1
C
      CTEMP = ' '
      CALL HOLCHR (IGBUFF(4), 1, 8, CTEMP, 1)
      call strcpy(C1UNIT, CTEMP)
      CALL HOLCHR (IGBUFF(6), 1, 8, CTEMP, 1)
      call strcpy(C1TYPE, CTEMP)
      CALL HOLCHR (IGBUFF(8), 1, 8, CTEMP, 1)
      call strcpy(C2UNIT, CTEMP)
      CALL HOLCHR (IGBUFF(10), 1, 8, CTEMP, 1)
      call strcpy(C2TYPE, CTEMP)
C
C
C
      LABEL = .FALSE.
	NH = (NCURVE*3) + 11
	IF (NIHEAD.GT.NH) THEN
	   NH = NH + 1
	   NWDS = IGBUFF(NH)
	   N = NH + NWDS
	   IF (NIHEAD.GE.N) THEN
	      NH = NH + 1
            CPPATH = ' '
	      LABEL = .TRUE.
            DO 100 I=1, NCURVE
               IF ((NH+ NWDS).GT.NGBUFF) GO TO 800
	         CALL HOL2CH(IGBUFF(NH), CPPATH, NWDS)
               IF (I.GT.KLABEL) THEN
               IF ((MLEVEL.GT.1).AND.(I.GT.1)) WRITE(MUNIT, 180) NCURVE,
     *             KLABEL
                  GO TO 800
               ENDIF
	         CLABEL(I) = CPPATH
	         NH = NH + NWDS
 100        CONTINUE
         ENDIF
	ELSE
	   CTEMP = ' '
         DO 200 I=1,NCURVE
            JLOC = (I * 3) + 9
            IF ((JLOC+2.GT.NIHEAD).OR.(I.GT.KLABEL)) THEN
            IF ((MLEVEL.GT.1).AND.(I.GT.1)) WRITE (MUNIT, 180) NCURVE,
     *                                      KLABEL
 180  FORMAT (' -----DSS---zrpd6;  CAUTION:  More Lables Stored than',
     * ' Buffer Size for Retrieving',/,' Number Stored:',I6,
     * ',  Space Available:',I6)
            GO TO 800
         ENDIF
         CALL HOLCHR (IGBUFF(JLOC), 1, 12, CTEMP, 1)
         call strcpy(CLABEL(I), CTEMP)
         IF (CTEMP(1:8).NE."        ") LABEL = .TRUE.
 200  CONTINUE
      ENDIF
C
C
 800  CONTINUE
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, LFOUND, LABEL, ISTAT
 820  FORMAT(T10,'----- Exit zrpd6, Number of data values retrieved:',
     * I7,/,T20,'Found:',L2,',  Labels:',L2,',  Status:',I4)
      !---------------------------------------------------------!
      ! convert values to requested vertical datum if necessary !
      !---------------------------------------------------------!
      call pathnameIsElevPd(cpath, paramIsElev)
      l_indElev = (paramIsElev.eq.1).or.(paramIsElev.eq.3)
      l_depElev = (paramIsElev.eq.2).or.(paramIsElev.eq.3)
      if (l_indElev.or.l_depElev) then
        !------------------------------------------------------!
        ! paired data has elevation in ordinates and/or values !
        !------------------------------------------------------!
        call zinqir(ifltab, 'VDTM', cvdatum1, ivdatum1)
        if (cvdatum1.ne.CVD_UNSET) then
          !----------------------------------------!
          ! we possibly need to convert the values !
          !----------------------------------------!
          call get_user_header_param(iuhead, nuhead,
     *      VERTICAL_DATUM_INFO_PARAM, vdiStr)
          if (len_trim(vdiStr).gt.0) then
            !-----------------------------------------------------------!
            ! we retrieved a user header and it has vertical datum info !
            !-----------------------------------------------------------!
            call stringToVerticalDatumInfo(
     *        vdiStr,
     *        errMsg,
     *        nativeDatum,
     *        unit,
     *        offsetNgvd29,
     *        l_Ngvd29Estimated,
     *        offsetNavd88,
     *        l_Navd88Estimated)
            if (errMsg.ne." ") then
              if (mlevel.ge.1) then
                write (munit,'(/,a,a,/,a,/,a)')
     *            ' *****DSS*** zrpdi6:  ERROR  - ',
     *            errMsg(1:len_trim(errMsg)),
     *            ' Cannot convert from native datum.',
     *            ' Elevations were not converted.'
              end if
              istat = 13
              return
            end if
            !--------------------------------------------!
            ! add the requested datum to the user header !
            !--------------------------------------------!
            iuhead_copy = 0
            max_copy_len = min(kuhead, size(iuhead_copy))
            iuhead_copy(max_copy_len) = iuhead(max_copy_len)
            call set_user_header_param(iuhead, nuhead, kuhead, 
     *        VERTICAL_DATUM_PARAM, cvdatum1, istat)
            if (istat.ne.0) then
              if (mlevel.ge.1) then
                write (munit,'(/,a,a,/,a)')
     *          ' *****DSS*** zrpdi6:  ERROR  - VERTICAL DATUM',
     *          ' TRUNCATED',
     *          ' Elevations were not converted.'
                
              end if
              istat = 13
              return
            end if
            max_copy_len = min(kuhead, size(iuhead_copy))
            iuhead(max_copy_len) = iuhead_copy(max_copy_len)
            !--------------------------------------!
            ! get the vertical datum offset to use !
            !--------------------------------------!
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
              if (vertDatumOffset.eq.
     *          UNDEFINED_VERTICAL_DATUM_VALUE) then
                if (mlevel.ge.1) then
                  write (munit,'(/,a,a,a,a,a,/,a)')
     *            ' *****DSS*** zrpdi6:  ERROR  - NO VERTICAL DATUM',
     *            ' OFFSET for ',nativeDatum(1:len_trim(nativeDatum)),
     *            ' to ',cvdatum1(1:len_trim(cvdatum1)),
     *            ' Elevations were not converted.'
                end if
                istat = 13
                return
              end if
              if (l_indElev) then
                !------------------------------------------------------------!
                ! convert the vertical datum offset to the units of the data !
                !------------------------------------------------------------!
                call getoffset(vertDatumOffset, unit, c1unit)
                if (vertDatumOffset.eq.
     *            UNDEFINED_VERTICAL_DATUM_VALUE) then
                  if (mlevel.ge.1) then
                    write (munit,'(/,a,a,a,a,a,a,/,a)')
     *              ' *****DSS*** zrpdi6:  ERROR  - ',
     *              'INVALID DATA UNIT (', c1unit(1:len_trim(c1unit)),
     *              ') OR OFFSET UNIT (', unit(1:len_trim(unit)),
     *              ') FOR VERTICAL DATUM CONVERSION',
     *              ' Elevations were not converted.'
                  end if
                  istat = 13
                  return
                end if
                !-----------------------------------!
                ! add the offset to the data values !
                !-----------------------------------!
                if (ldouble) then
                  do i = 1, nord
                    dvalues(i) = dvalues(i) + vertDatumOffset
                  end do
                else
                  do i = 1, nord
                    svalues(i) = svalues(i) + vertDatumOffset
                  end do
                end if
              end if
              if (l_depElev) then
                !------------------------------------------------------------!
                ! convert the vertical datum offset to the units of the data !
                !------------------------------------------------------------!
                call getoffset(vertDatumOffset, unit, c2unit)
                if (vertDatumOffset.eq.
     *            UNDEFINED_VERTICAL_DATUM_VALUE) then
                  if (mlevel.ge.1) then
                    write (munit,'(/,a,a,a,a,a,a,/,a)')
     *              ' *****DSS*** zrpdi6:  ERROR  - ',
     *              'INVALID DATA UNIT (', c2unit(1:len_trim(c2unit)),
     *              ') OR OFFSET UNIT (', unit(1:len_trim(unit)),
     *              ') FOR VERTICAL DATUM CONVERSION',
     *              ' Elevations were not converted.'
                  end if
                  istat = 13
                  return
                end if
                !-----------------------------------!
                ! add the offset to the data values !
                !-----------------------------------!
                if (ldouble) then
                  do i = nord+1, (ncurve+1) * nord
                    dvalues(i) = dvalues(i) + vertDatumOffset
                  end do
                else
                  do i = nord+1, (ncurve+1) * nord
                    svalues(i) = svalues(i) + vertDatumOffset
                  end do
                end if
              end if
            end if
          end if
        end if
      end if
      RETURN
C
 900  CONTINUE
      NVALS = 0
      NUHEAD = 0
      GO TO 800
C
 910  FORMAT(' -----DSS---ZRPD;  CAUTION:  More data stored than',
     * ' Buffer Size for Retrieving',/,' Pathname: ',A,
     * ' Number Stored:',I8,',  Space Available:',I8)
C
      END

