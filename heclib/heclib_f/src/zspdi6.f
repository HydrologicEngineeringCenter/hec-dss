      SUBROUTINE zspdi6 (IFLTAB, CPATH, NORD, NCURVE, IHORIZ,
     * C1UNIT, C1TYPE, C2UNIT, C2TYPE, SVALUES, DVALUES,
     * LDOUBLE, CLABEL, LABEL, IUHEAD, NUHEAD, IPLAN, ISTAT)
C
C
C     Internal Store Paired DATA
C
C     Written by Bill Charley
C
C
C     IPLAN:  10 - Alloocated space for a series of curves,
C     where each of the curves is written individually at separate times.
C     The SVALUES or DVALUES array must contain (only) ordinates.
C     NORD, NCURVE, IHORIZ, C1UNIT, C1TYPE, C2UNIT, C2TYPE, LDOUBLE,
C     LABEL, IUHEAD, NUHEAD all must be set at this time.
C     The amount of space to allocate is based on NORD and NCURVE.
C     No curve data is set for this call.
C
C     IPLAN 11 - Write the coordiantes (not ordinates) for an individual
C     cruve that has been allocated with plan 10 above.
C     NCURVE must be the curve number (starting with 1).
C     The SVALUES or DVALUES array must contain (only) coordiantes for
C     that one curve.  If LABEL, then CLABEL must contain the label
C     for that curve.  You must store the same size (float) as allocated.
C     NORD, IHORIZ, C1UNIT, C1TYPE, C2UNIT, C2TYPE, LDOUBLE,
C     IUHEAD, NUHEAD are ignored.
C
C
      INTEGER IFLTAB(*), IUHEAD(*)
      CHARACTER CPATH*(*), CLABEL(*)*(*)
      CHARACTER C1UNIT*(*), C1TYPE*(*), C2UNIT*(*), C2TYPE*(*)
      CHARACTER CTEMP*100
      REAL SVALUES(*)
      DOUBLE PRECISION DVALUES(*)
      LOGICAL LABEL, LFOUND, LDOUBLE
      INTEGER MAXLABEL
C
C     Vertical datum varible dimensions
      character*400 fileVdiStr, dataVdiStr, errMsg
      character*16 unit, cvdatum1, cvdatum2
      character*16 cvdatum_ind, cvdatum_dep
      double precision vertDatumOffset_ind, vertDatumOffset_dep
      logical*4 l_indElev, l_depElev, l_ordsModified, l_valsModified
      integer nuhead_copy1, iuhead_copy1(100), paramIsElev
      integer nuhead_copy2, iuhead_copy2(100)
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
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'verticalDatumFortran.h'
C
C
      CALL CHRLNB (CPATH, NPATH)
C
C     If a debug level is on, print out information
      IF (MLEVEL.GE.7) THEN
      WRITE (MUNIT,20) NORD, NCURVE, IHORIZ, LABEL, IPLAN, NUHEAD,
     * CPATH(1:NPATH)
 20   FORMAT (T5,'----- Enter zspdi6 -----',/,
     * T11,'NORD:',I6,',  NCURVE:',I4,',  IHORIZ:',I4,/,
     * T11,'LABEL: ',L1,',  IPLAN:',I3,',  NUHEAD:',I5,/,
     * T11,'Pathname: ',A)
      WRITE (MUNIT,21) C1UNIT, C1TYPE, C2UNIT, C2TYPE
 21   FORMAT(T5,'X Units: ',A,'  X Type: ',A,/,
     *       T5,'Y Units: ',A,'  Y Type: ',A)
      IF (LABEL) WRITE (MUNIT, 22) CLABEL(1)
 22   FORMAT (5X,'First label: ',A)
      ENDIF
C
C
C     Check that IFLTAB is valid (e.g., the DSS file is open)
      IF (IFLTAB(1).NE.6) CALL zerror6(IFLTAB, 5, 'ZSPDI6',
     * 0, IFLTAB, ' ', 0, ' ',0)
      !--------------------------------------------------------------------!
      ! make a copy of the user header becuase we have to be able to know  !
      ! its size when passing to user header manipulation routintes and we !
      ! cant know the size of an assumed-size array                        !
      !--------------------------------------------------------------------!
      if (nuhead.gt.size(iuhead_copy2)) then
        if (mlevel.ge.1) then
          write (munit,'(/,a,/,a,i5,/,a,i5)')
     *    ' *****DSS*** zspdi6:  WARNING  - USER HEADER TRUNCATED',
     *    ' Origninal size = ', nuhead,
     *    ' Truncated size = ', size(iuhead_copy2)
        end if
      end if
      nuhead_copy2 = min(size(iuhead_copy2), nuhead)
      iuhead_copy2 = 0
      iuhead_copy2(:nuhead_copy2) = iuhead(:nuhead_copy2)
      !--------------------------------------------------------------!
      ! convert the values to the native vertical datum if necessary !
      !--------------------------------------------------------------!
      l_ordsModified = .false.
      l_valsModified = .false.
      call pathnameIsElevPd(cpath, paramIsElev)
      l_indElev = (paramIsElev.eq.1).or.(paramIsElev.eq.3)
      l_depElev = (paramIsElev.eq.2).or.(paramIsElev.eq.3)
      if (l_indElev.or.l_depElev) then
        !----------------------------------!
        ! elevation in ordinates or values !
        !----------------------------------!
        !----------------------------------------------!
        ! get the VDI on in the file for this pathname !
        !----------------------------------------------!
        fileVdiStr = ' '
        dataVdiStr = ' '
        cvdatum1 = ' '
        iiihead = 0
        inihead = 0
        iichead = 0
        inchead = 0
        iidata  = 0
        indata  = 0
        call zreadx6(
     *    ifltab,              !IFLTAB
     *    cpath(1:npath),      !CPATH
     *    iiihead,             !IIHEAD
     *    0,                   !KIHEAD
     *    inihead,             !NIHEAD
     *    iichead,             !ICHEAD
     *    0,                   !KCHEAD
     *    inchead,             !NCHEAD
     *    iuhead_copy1,        !IUHEAD
     *    size(iuhead_copy1),  !KUHEAD
     *    nuhead_copy1,        !NUHEAD
     *    iidata,              !IDATA
     *    0,                   !KDATA
     *    indata,              !NDATA
     *    0,                   !IPLAN
     *    lfound)              !LFOUND
        if (lfound.and.(nuhead_copy1.gt.0)) then
          call get_user_header_param(
     *      iuhead_copy1,
     *      nuhead_copy1,
     *      VERTICAL_DATUM_INFO_PARAM,
     *      fileVdiStr)
        end if
        !------------------------------!
        ! get the VDI of incoming data !
        !------------------------------!
        call normalizeVdiInUserHeader(
     *    iuhead_copy2,
     *    nuhead_copy2,
     *    size(iuhead_copy2),
     *    errMsg)
        call get_user_header_param(
     *    iuhead_copy2,
     *    nuhead_copy2,
     *    VERTICAL_DATUM_INFO_PARAM,
     *    dataVdiStr)
        !-------------------------------------------------!
        ! get the current vertical datum of incoming data !
        !-------------------------------------------------!
        ! ==> first, get any default vertical datum
        call zinqir6(IFLTAB, 'VDTM', cvdatum1, ivdatum1)
        ! ==> next, override the default with any datum in the user header
        call get_user_header_param(
     *    iuhead_copy2,
     *    nuhead_copy2,
     *    VERTICAL_DATUM_PARAM,
     *    cvdatum2)
        if (cvdatum2.ne." ") then
          cvdatum1 = cvdatum2
          ! remove current vertical datum from user header so it is not saved to disk
          call remove_user_header_param(
     *      iuhead_copy2,
     *      nuhead_copy2,
     *      size(iuhead_copy2),
     *      VERTICAL_DATUM_PARAM)
        end if
        ! ==> finally, override both with the unit spec
        cvdatum_ind = cvdatum1
        cvdatum_dep = cvdatum1
        ! override both with the unit spec
        if (l_indElev) then
          call crack_unit_spec(c1unit, unit, cvdatum2)
          if (cvdatum2.ne." ") then
            c1unit = unit(1:len_trim(unit))
            cvdatum_ind = cvdatum2
          end if
        end if
        if (l_depElev) then
          call crack_unit_spec(c2unit, unit, cvdatum2)
          if (cvdatum2.ne." ") then
            c2unit = unit(1:len_trim(unit))
            cvdatum_dep = cvdatum2
          end if
        end if
        !----------------------------------------------------------------!
        ! process the VDIs and get the offsets to use (or error message) !
        !----------------------------------------------------------------!
        if (l_indElev) then
          ! ==> for independent parameter  
          call processStorageVdis(
     *      vertDatumOffset_ind,
     *      errMsg,
     *      fileVdiStr,
     *      dataVdiStr,
     *      cvdatum_ind,
     *      lfound,
     *      c1unit)
          if (errMsg.ne.' ') then
            if (mlevel.ge.1) then
              write (munit,'(/,a,a)')
     *        ' *****DSS*** zspdi6: ',
     *        errMsg(:len_trim(errMsg))
            end if
            istat = 13
            return
          end if
        end if
        if (l_depElev) then
          ! ==> for dependent parameter  
          call processStorageVdis(
     *      vertDatumOffset_dep,
     *      errMsg,
     *      fileVdiStr,
     *      dataVdiStr,
     *      cvdatum_dep,
     *      lfound,
     *      c2unit)
          if (errMsg.ne.' ') then
            if (mlevel.ge.1) then
              write (munit,'(/,a,a)')
     *        ' *****DSS*** zspdi6: ',
     *        errMsg(:len_trim(errMsg))
            end if
            istat = 13
            return
          end if
        end if    
        !---------------------------------------!
        ! modify the incoming data as necessary !
        !---------------------------------------!
        if (l_indElev) then
          if (vertDatumOffset_ind.ne.0) then
            if (iplan.ne.11) then
              !----------------!
              ! ordinates only !
              !----------------!
              l_ordsModified = .true.
              if (ldouble) then
                do i = 1, nord
                  dvalues(i) = dvalues(i) - vertDatumOffset_ind
                end do
              else
                do i = 1, nord
                  svalues(i) = svalues(i) - vertDatumOffset_ind
                end do
              end if
            end if
          end if
        end if
        if (l_depElev) then
          if (vertDatumOffset_dep.ne.0) then
            if (iplan.eq.11) then
              !---------------------------------------------!
              ! data has values for one curve, no ordinates !
              !---------------------------------------------!
              l_valsModified = .true.
              if (ldouble) then
                do i = 1, nord
                  dvalues(i) = dvalues(i) - vertDatumOffset_dep
                end do
              else
                do i = 1, nord
                  svalues(i) = svalues(i) - vertDatumOffset_dep
                end do
              end if
            elseif (iplan.ne.10) then
              !-------------------------------------------!
              ! data has ordinates and one or more curves !
              !-------------------------------------------!
              l_ordsModified = .true.
              l_valsModified = .true.
              if (ldouble) then
                do i = nord+1, (ncurve+1)*nord
                  dvalues(i) = dvalues(i) - vertDatumOffset_dep
                end do
              else
                do i = nord+1, (ncurve+1)*nord
                  svalues(i) = svalues(i) - vertDatumOffset_dep
                end do
              end if
            end if
          end if
        end if
        !--------------------------------------------------------------------------!
        ! re-store the user header from the record if no user header was passed in !
        !--------------------------------------------------------------------------!
        if (nuhead_copy2.le.1.and.nuhead_copy1.gt.0) then
          nuhead_copy2 = nuhead_copy1
          iuhead_copy2(:nuhead_copy2) = iuhead_copy1(:nuhead_copy1)
        end if
      end if
C
C
      IF (IPLAN.EQ.11) THEN
C        Store only a single coordinate array (one curve) and label
         LWRITE = .TRUE.
         CALL ZMULTU6 ( IFLTAB, .TRUE., .TRUE.)
         CALL ZRDINF6 (IFLTAB, CPATH, NH, ND, ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL ZMULTU6 ( IFLTAB, .FALSE., .TRUE.)
            LWRITE = .FALSE.
            GO TO 900
         ENDIF

         IF (LDOUBLE) THEN
            N = NORD  *2
            IADD = INFO(NPPWRD+KIADAT) + (NCURVE * NORD * 2)
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
            CALL ZPTREC6(IFLTAB, DVALUES, N, IADD, .FALSE.)
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
         ELSE
            IADD = INFO(NPPWRD+KIADAT) + (NCURVE * NORD)
            CALL ZPTREC6(IFLTAB, SVALUES, NORD, IADD, .FALSE.)
         ENDIF
         NVALS = NORD

C       Now store the label
        IF (LABEL) THEN
            CTEMP = CLABEL(1)
            IADD = INFO(NPPWRD+KIAIHE)
            NIHEAD = INFO(NPPWRD+KINIHE)
            CALL zgtrec6(IFLTAB, IGBUFF, NIHEAD, IADD, .FALSE.)
            CALL CHRHOL (CTEMP, 1, 12, IGBUFF((NCURVE*3)+9), 1)

C           Do we have extended labels?
            NTOTALCURV = IGBUFF(2)
            NH = (NTOTALCURV*3) + 11
	      IF (NIHEAD.GT.NH) THEN
	         NH = NH + 1
C              NWDS is the number of words for each extended label
	         NWDS = IGBUFF(NH)
	         N = NH + NWDS
	         IF ((NIHEAD.GE.N).AND.(NWDS.LT.26)) THEN
	             NH = NH + 1
                   NH = NH + (NCURVE - 1) * NWDS
	             CALL CH2HOL (CTEMP, IGBUFF(NH), NWDS)
	          ENDIF
             ENDIF
          CALL ZPTREC6(IFLTAB, IGBUFF, NIHEAD, IADD, .FALSE.)
        ENDIF

         CALL ZMULTU6( IFLTAB, .FALSE., .TRUE.)
         LWRITE = .FALSE.
         GO TO 800
      ENDIF
C
C
C     Error checking
      IF (NORD.LT.1) GO TO 905
      IF (NCURVE.LT.1) GO TO 910
      IF (NCURVE.GT.100) GO TO 920
C
      IGBUFF(1) = NORD
      IGBUFF(2) = NCURVE
      IGBUFF(3) = 1
      IF (IHORIZ.EQ.2) IGBUFF(3) = 2
C
      CTEMP = C1UNIT
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(4), 1)
      CTEMP = C1TYPE
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(6), 1)
      CTEMP = C2UNIT
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(8), 1)
      CTEMP = C2TYPE
      CALL CHRHOL (CTEMP, 1, 8, IGBUFF(10), 1)
C
      NVALS = (NORD * (NCURVE + 1))
      NIHEAD = 11
C
C
      IF (IPLAN.EQ.10) THEN
C       Use space from the time series buffer array and store zeros
        IF (LDOUBLE) THEN
            JTYPE = 205
             NTOT = NVALS * 2
         ELSE
             JTYPE = 200
             NTOT = NVALS
         ENDIF

         IF (MLEVEL.GE.7) THEN
            WRITE (MUNIT,70) NVALS, NTOT, KLBUFF
 70         FORMAT('    Number values to store:',I7,
     *      ' Size to store:',I7,' Size Limit:',I7)
         ENDIF
         IF (NTOT.GT.KLBUFF) GO TO 930

C        Store a zeroed array
         DO 90 I=1,NTOT
            BUFF(I) = 0.0
 90      CONTINUE

         IF (LABEL) THEN
C           Store blank labels
            CTEMP = ' '
            NIHEAD = (NCURVE*3) + 11
            DO 91 I=1,NCURVE
              CALL CHRHOL (CTEMP, 1, 12, IGBUFF((I*3)+9), 1)
 91        CONTINUE
C
C       Do we have extended labels?  If so, store their size in words
C       and the full labels following the standard labels (for
C       compatibility)
          CALL CHRLNB(CLABEL(1), MAXLABEL)
          IF (MAXLABEL.GT.12) THEN
	      NWDS = ((MAXLABEL - 1) / 4) + 1
	      NIHEAD = NIHEAD + 1
	      IGBUFF(NIHEAD) = NWDS
            IF ((NIHEAD + (NCURVE * NWDS)).GT.NGBUFF) GO TO 900
	      DO 92 I=1,NCURVE
              NIHEAD = NIHEAD + 1
	        CALL CH2HOL (CTEMP, IGBUFF(NIHEAD), NWDS)
	        NIHEAD = NIHEAD + NWDS - 1
 92         CONTINUE
	    ENDIF
        ENDIF

         call normalizeVdiInUserHeader(
     *     iuhead_copy2,
     *     nuhead_copy2,
     *     size(iuhead_copy2),
     *     errMsg)
         CALL zwritex6(IFLTAB, CPATH, NPATH, IGBUFF, NIHEAD,
     *   ICHEAD, 0, iuhead_copy2, nuhead_copy2, BUFF, NTOT, JTYPE,
     *   0, ISTAT, LFOUND)
         IF (ISTAT.NE.0) GO TO 900

C        Now store the ordinates
         LWRITE = .TRUE.
         CALL ZMULTU6( IFLTAB, .TRUE., .TRUE.)
         CALL ZRDINF6(IFLTAB, CPATH, NH, ND, ISTAT)
         IF (ISTAT.NE.0) THEN
            CALL ZMULTU6( IFLTAB, .FALSE., .TRUE.)
            LWRITE = .FALSE.
            GO TO 900
         ENDIF

         IF (LDOUBLE) THEN
            N = NORD  *2
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
            CALL ZPTREC6(IFLTAB,DVALUES, N, INFO(NPPWRD+KIADAT),.FALSE.)
            IF (IFLTAB(KDSWAP).NE.0) CALL ZDSWAP6(DVALUES, N)
         ELSE
            CALL ZPTREC6(IFLTAB, SVALUES, NORD, INFO(NPPWRD+KIADAT),
     *                   .FALSE.)
         ENDIF

         CALL ZMULTU6( IFLTAB, .FALSE., .TRUE.)
         LWRITE = .FALSE.
         GO TO 800

      ENDIF
C
C     Normal write
C
      CTEMP = ' '
	    MAXLABEL = 0
      IF (LABEL) THEN
        NIHEAD = (NCURVE*3) + 11
        DO 100 I=1,NCURVE
          CTEMP = CLABEL(I)
	    CALL CHRLNB(CLABEL(I), NLAB)
	    IF (NLAB.GT.MAXLABEL) MAXLABEL = NLAB
          CALL CHRHOL (CTEMP, 1, 12, IGBUFF((I*3)+9), 1)
 100    CONTINUE
      ENDIF

C
C
C     Do we have extended labels?  If so, store their size in words
C     and the full labels following the standard labels (for
C     compatibility)
C
      IF (MAXLABEL.GT.12) THEN
	    NWDS = ((MAXLABEL - 1) / 4) + 1
	    NIHEAD = NIHEAD + 1
	    IGBUFF(NIHEAD) = NWDS
	    DO 120 I=1,NCURVE
	      CTEMP = CLABEL(I)
            NIHEAD = NIHEAD + 1
	      CALL CH2HOL (CTEMP, IGBUFF(NIHEAD), NWDS)
	      NIHEAD = NIHEAD + NWDS - 1
 120  CONTINUE
	    ENDIF
C
      call normalizeVdiInUserHeader(
     *  iuhead_copy2,
     *  nuhead_copy2,
     *  size(iuhead_copy2),
     *  errMsg)
      iplan_write = 0
C     IPLAN = 0   Always write
C     IPLAN = 1   Only write if new record
C     IPLAN = 2   Only write if old record
      
      IF (LDOUBLE) THEN
         JTYPE = 205
         N = NVALS * 2
C        Swap words on unix to keep compatitable with PC
         IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DVALUES, N)
         CALL zwritex6 (IFLTAB, CPATH, NPATH, IGBUFF, NIHEAD,
     *   ICHEAD, 0, iuhead_copy2, nuhead_copy2, DVALUES, N, JTYPE,
     *   iplan_write, ISTAT, LFOUND)
C        Swap back so we don't mess up the user's data
         IF (IFLTAB(KDSWAP).NE.0) CALL zdswap6(DVALUES, N)
      ELSE
         JTYPE = 200
         CALL zwritex6 (IFLTAB, CPATH, NPATH, IGBUFF, NIHEAD,
     *   ICHEAD, 0, iuhead_copy2, nuhead_copy2, SVALUES, NVALS,
     *   JTYPE, iplan_write, ISTAT, LFOUND)
      ENDIF
C

C
C
 800  CONTINUE
      !------------------------------------------!
      ! restore the original values if necessary !
      !------------------------------------------!
      if (l_ordsModified) then
        if (ldouble) then
          do i = 1, nord
            dvalues(i) = dvalues(i) + vertDatumOffset_ind
          end do
        else
          do i = 1, nord
            svalues(i) = svalues(i) + vertDatumOffset_ind
          end do
        endif
      end if
      if (l_valsModified) then
        if (iplan.eq.11) then
          if (ldouble) then
            do i = 1, nord
              dvalues(i) = dvalues(i) + vertDatumOffset_dep
            end do
          else
            do i = 1, nord
              svalues(i) = svalues(i) + vertDatumOffset_dep
            end do
          endif
        else
          if (ldouble) then
            do i = nord+1, (ncurve+1) * nord
              dvalues(i) = dvalues(i) + vertDatumOffset_dep
            end do
          else
            do i = nord+1, (ncurve+1) * nord
              svalues(i) = svalues(i) + vertDatumOffset_dep
            end do
          endif
        end if
      end if
      IF (MLEVEL.GE.7) WRITE (MUNIT,820) NVALS, ISTAT
 820  FORMAT(T5,'----- Exit zspdi6, Number of data values ',
     * 'stored:',I7,',  Status:',I4,/)
C
      RETURN
C
C
 900  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 901) CPATH(1:NPATH)
 901  FORMAT (/,' *** ERROR:  zspdi6;  Error writting record',
     * /,' Pathname: ',A,/)
      GO TO 800
C
 905  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 906) NORD, CPATH(1:NPATH)
 906  FORMAT (/,' *** ERROR:  zspdi6;  The Number of Ordinates is Less',
     * ' than One ***',/,' Number Supplied:',I6,/,' Pathname: ',A,/)
      ISTAT = -4
      GO TO 800
C
 910  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 911) NCURVE, CPATH(1:NPATH)
 911  FORMAT (/,' *** ERROR:  zspdi6;  The Number of Curves is Less',
     * ' than One ***',/,' Number Supplied:',I6,/,' Pathname: ',A,/)
      ISTAT = -5
      GO TO 800
C
 920  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 921) NCURVE, CPATH(1:NPATH)
 921  FORMAT (/,' *** ERROR:  zspdi6;  The Number of Curves is Greater',
     * ' than 100 ***',/,' Number Supplied:',I6,'(Up to 100 curves may',
     * ' be stored in one record)',/,' Pathname: ',A)
      ISTAT = -5
      GO TO 800
C
C
 930  CONTINUE
      IF (MLEVEL.GE.1) WRITE (MUNIT, 931)  CPATH(1:NPATH)
 931  FORMAT (/,' *** ERROR:  zspdi6;  Exceeded size limit for number',
     * ' of data ***',/,' Pathname: ',A)
      WRITE (MUNIT,932) NVALS, NTOT, KLBUFF
 932  FORMAT('    Number values to store:',I7,
     *      ' Size to store:',I7,' Size Limit:',I7)
      ISTAT = -5
      GO TO 800
C
      END

