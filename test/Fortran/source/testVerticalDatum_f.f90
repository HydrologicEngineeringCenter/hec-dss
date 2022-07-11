module modVerticalDatumInfo
    implicit none

    real (kind=8), parameter :: UNDEFINED_VERTICAL_DATUM_VALUE = -3.4028234663852886e+38
    integer, parameter :: cverticalDatumLen = 16
    integer, parameter :: unitLen = 16
    integer, parameter :: IVD_UNSET  = 0
    integer, parameter :: IVD_NAVD88 = 1
    integer, parameter :: IVD_NGVD29 = 2
    integer, parameter :: IVD_OTHER  = 3
    integer, parameter :: IVD_LOCAL  = 3
    character (len=17), parameter :: VERTICAL_DATUM_INFO_PARAM = 'verticalDatumInfo'
    character (len=13), parameter :: VERTICAL_DATUM_PARAM = 'verticalDatum'
    character (len=cverticalDatumLen), parameter :: CVD_UNSET  = 'UNSET  '
    character (len=cverticalDatumLen), parameter :: CVD_NAVD88 = 'NAVD-88'
    character (len=cverticalDatumLen), parameter :: CVD_NGVD29 = 'NGVD-29'
    character (len=cverticalDatumLen), parameter :: CVD_OTHER  = 'OTHER  '
    character (len=cverticalDatumLen), parameter :: CVD_LOCAL  = 'OTHER  '

    type verticalDatumInfo
        real (kind=8) :: offsetToNgvd29, offsetToNavd88
        character (len=cverticalDatumLen) :: nativeDatum
        character (len=unitLen) :: unit
        logical :: offsetToNgvd29IsEstimate, offsetToNavd88IsEstimate
    end type verticalDatumInfo

    interface
        subroutine verticalDatumInfoToString( &
            outputStr,                        &
            errorMessage,                     &
            nativeDatum,                      &
            unit,                             &
            offsetNgvd29,                     &
            offsetNgvd29IsEstimate,           &
            offsetNavd88,                     &
            offsetNavd88IsEstimate,           &
            generateCompressed)
            character (len = *),  intent(out) :: outputStr
            character (len = *),  intent(out) :: errorMessage
            character (len = *),  intent(in)  :: nativeDatum
            character (len = *),  intent(in)  :: unit
            real      (kind = 8), intent(in)  :: offsetNgvd29
            logical   (kind = 4), intent(in)  :: offsetNgvd29IsEstimate
            real      (kind = 8), intent(in)  :: offsetNavd88
            logical   (kind = 4), intent(in)  :: offsetNavd88IsEstimate
            logical   (kind = 4), intent(in)  :: generateCompressed
        end subroutine verticalDatumInfoToString

        subroutine stringToVerticalDatumInfo( &
            inputStr,                         &
            errorMessage,                     &
            nativeDatum,                      &
            unit,                             &
            offsetNgvd29,                     &
            offsetNgvd29IsEstimate,           &
            offsetNavd88,                     &
            offsetNavd88IsEstimate)
            character (len = *),  intent(in)  :: inputStr
            character (len = *),  intent(out) :: errorMessage
            character (len = *),  intent(out) :: nativeDatum
            character (len = *),  intent(out) :: unit
            real      (kind = 8), intent(out) :: offsetNgvd29
            logical   (kind = 4), intent(out) :: offsetNgvd29IsEstimate
            real      (kind = 8), intent(out) :: offsetNavd88
            logical   (kind = 4), intent(out) :: offsetNavd88IsEstimate
        end subroutine stringToVerticalDatumInfo

        logical function unitIsFeet(unit)
            character (len = *) :: unit
        end function unitIsFeet

        logical function unitIsMeters(unit)
            character (len = *) :: unit
        end function unitIsMeters
    end interface

    contains
        subroutine initVerticalDatumInfo(vdi)
            type(verticalDatumInfo) vdi
            vdi%offsetToNavd88 = UNDEFINED_VERTICAL_DATUM_VALUE
            vdi%offsetToNgvd29 = UNDEFINED_VERTICAL_DATUM_VALUE
            vdi%nativeDatum = ' '
            vdi%offsetToNavd88IsEstimate = .false.
            vdi%offsetToNgvd29IsEstimate = .false.
        end subroutine initVerticalDatumInfo

        integer function byteCountToIntCount(byteCount)
            integer :: byteCount
            if (byteCount == 0) then
                byteCountToIntCount = 0
            else
                byteCountToIntCount = (byteCount-1) / 4 + 1
            end if
        end function byteCountToIntCount

        integer function iswap(iin)
            integer (kind=4) :: iin
            call zswap6(iin, iswap)
        end function iswap

        logical function isBigEndian()
            integer i;
            call bigEndian(i)
            isBigEndian = i.eq.1
        end function isBigEndian

end module modVerticalDatumInfo

integer function test_vertical_datums_f()
    implicit none
    call testUserHeaderOps()
    call testStoreRetrieveTimeSeries()
    call testStoreRetrievePairedData()

    test_vertical_datums_f = 0
end function test_vertical_datums_f

subroutine testUserHeaderOps
    implicit none

    character (len=100) :: cheader, cvalue
    character (len=72)  :: cheaderShort 
    integer   (kind=4)  :: iheader(25), iheaderShort(18), nhead, status

    equivalence (cheader, iheader)
    equivalence (cheaderShort, iheaderShort)

    cheader = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'

    call get_user_header_param(iheader, size(iheader), 'firstParam', cvalue)
    call assert(cvalue.eq.'firstValue')
    call get_user_header_param(iheader, size(iheader), 'secondParam', cvalue)
    call assert(cvalue.eq.'secondValue')
    call get_user_header_param(iheader, size(iheader), 'thirdParam', cvalue)
    call assert(cvalue.eq.'thirdValue')
    nhead = size(iheader)
    call set_user_header_param(iheader, nhead, size(iheader), 'firstParam', 'FIRSTValue', status)
    call assert(status.eq.0)
    call get_user_header_param(iheader, nhead, 'firstParam', cvalue)
    call assert(cvalue.eq.'FIRSTValue')
    call set_user_header_param(iheader, nhead, size(iheader), 'secondParam', '2ndValue', status)
    call assert(status.eq.0)
    call get_user_header_param(iheader, nhead, 'secondParam', cvalue)
    call assert(cvalue.eq.'2ndValue')
    call set_user_header_param(iheader, nhead, size(iheader), 'thirdParam', 'THIRDValue', status)
    call assert(status.eq.0)
    call get_user_header_param(iheader, nhead, 'thirdParam', cvalue)
    call assert(cvalue.eq.'THIRDValue')
    call set_user_header_param(iheader, nhead, size(iheader), 'fourthParam', '4thValue', status)
    call assert(status.eq.0)

    cheader = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'
    call remove_user_header_param(iheader, nhead, size(iheader), 'firstParam')
    call assert(cheader.eq.'secondParam:secondValue;thirdParam:thirdValue;')
    cheader = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'
    call remove_user_header_param(iheader, nhead, size(iheader), 'secondParam')
    call assert(cheader.eq.'firstParam:firstValue;thirdParam:thirdValue;')
    cheader = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'
    call remove_user_header_param(iheader, nhead, size(iheader), 'thirdParam')
    call assert(cheader.eq.'firstParam:firstValue;secondParam:secondValue;')

    cheaderShort = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'
    call set_user_header_param(iheaderShort, nhead, size(iheaderShort), 'fourthParam', '4thValue', status)
    call assert(status.ne.0)

end subroutine testUserHeaderOps

subroutine testStoreRetrieveTimeSeries()
    use modVerticalDatumInfo
    implicit none

    integer (kind=8)        :: ifltab(250)
    integer (kind=4)        :: status, numberValues, i, j, k, k2, k3, kk, l, m, n, o, p, ii, len, iVerticalDatum
    integer (kind=4)        :: quality(24), itimes(6,2),userHeader(100), userHeaderLen, count
    integer (kind=4)        :: intervalOffset, compressionMethod, timesRetrieved(24), baseDate
    integer (kind=4)        :: startDay, endDay, ihm2m
    real (kind=8)           :: dvalues(6,3), dvals(24), dvals_out(24)
    real (kind=4)           :: fvalues(6,3), fvals(24), fvals_out(24)
    character (len=300)     :: errmsg, vdiStr
    character (len=80)      :: filename(2)
    character (len=80)      :: pathnames(2,2)
    character (len=16)      :: unit(3), type, verticalDatums(3), cVerticalDatum
    character (len=32)      :: unitSpec
    character (len=16)      :: startDate(2), endDate(2)
    character (len=4)       :: startTime, endTime
    character (len=400)     :: userHeaderStr
    logical                 :: readQuality, qualityWasRead, expectSuccess
    type(verticalDatumInfo) :: vdi(2)

    equivalence (userHeader, userHeaderStr)

    ifltab = 0
    quality = 0.
    userHeader = 0
    dvals = 0.
    fvals = 0.
    readQuality = .true.

    itimes = reshape((/                                             &
        64035420, 64035480, 64035540, 64035600, 64035660, 64035720, &
        64080060, 64080120, 64080180, 64080240, 64080300, 64080360  &
        /),shape(itimes))

    dvalues = reshape((/                                   &
        1000.,1001.,1002.,1003.,1004.,1005.,               & ! ft
        304.8,305.1048,305.4096,305.7144,306.0192,306.324, & ! m
        1000.,1001.,1002.,1003.,1004.,1005.                & ! cfs
        /),shape(dvalues))

    fvalues = dvalues

    filename = (/'v6_f.dss', 'v7_f.dss'/)

    pathnames = reshape((/                     &
        '//TestTsLoc/Elev//1Hour/Doubles/   ', &
        '//TestTsLoc/Elev//1Hour/Floats/    ', &
        '//TestTsLoc/Elev//Ir-Month/Doubles/', &
        '//TestTsLoc/Elev//Ir-Month/Floats/ '  &
        /), shape(pathnames))

    unit = (/'ft ', 'm  ', 'cfs'/)

    type = 'INST-VAL'

    startDate = (/'01Oct2021', '01Nov2021'/)

    endDate = startDate

    startTime = '0100'

    endTime = '2400'

    verticalDatums = (/CVD_NAVD88, CVD_NGVD29, 'Pensacola       '/)

    do j = 1, 2
        call initVerticalDatumInfo(vdi(j))
        if (j == 1) then
            vdi(j)%nativeDatum = CVD_NGVD29
            vdi(j)%unit = 'ft'
            vdi(j)%offsetToNavd88 = 0.3855
            vdi(j)%offsetToNavd88IsEstimate = .true.
        else
            vdi(j)%nativeDatum = 'Pensacola'
            vdi(j)%unit = 'ft'
            vdi(j)%offsetToNavd88 = 1.457
            vdi(j)%offsetToNavd88IsEstimate = .true.
            vdi(j)%offsetToNgvd29 = 1.07
            vdi(j)%offsetToNgvd29IsEstimate = .false.
        end if
    end do
    !
    ! loop variables
    !
    ! i = DSS file version
    !     1 = DSS 6
    !     2 = DSS 7
    !
    ! j = vdi
    !     1 = NGVD-29 native
    !     2 = OTHER native with local datum named "Pensacola"
    !
    ! k = vertical datum
    !     1 = NAVD-88
    !     2 = NGVD-29
    !     3 = OTHER (Pensacola)
    !     k2  mod(k, 3) + 1
    !     k3  mod(k+1, 3) + 1
    !
    ! l = data units
    !     1 = ft
    !     2 = m
    !     3 = cfs (invalid for datum conversion)
    !
    ! m = vertical datum specification method
    !     1 = set default with zset
    !     2 = 1 plus override with user header
    !     3 = 2 plus override with unit spec
    !
    ! n = time series type
    !     1 = regular time series
    !     2 = irregular time series
    !
    ! o = data value type
    !     1 = doubles
    !     2 = floats
    !
    ! p = specify vertical datum info in user header on store
    !     1 = specify
    !     2 = don't specify (use previously stored)
    !
    call zset('MLVL', '', 1)
    count = 0
    do i = 1, 2
        call deletefile(filename(i), status)
        do j = 1, 2
            call verticalDatumInfoToString(       &
                vdiStr,                           &
                errmsg,                           &
                vdi(j)%nativeDatum,               &
                vdi(j)%unit,                      &
                vdi(j)%offsetToNgvd29,            &
                vdi(j)%offsetToNgvd29IsEstimate,  &
                vdi(j)%offsetToNavd88,            &
                vdi(j)%offsetToNavd88IsEstimate,  &
                .true.)
            call assert(errmsg == ' ')
            call stringToVerticalDatumInfo(      &
                vdiStr,                          &
                errmsg,                          &
                vdi(j)%nativeDatum,              &
                vdi(j)%unit,                     &
                vdi(j)%offsetToNgvd29,           &
                vdi(j)%offsetToNgvd29IsEstimate, &
                vdi(j)%offsetToNavd88,           &
                vdi(j)%offsetToNavd88IsEstimate)
            call assert(errmsg == ' ')
            do k = 1, 3
                k2 = mod(k, 3) + 1
                k3 = mod(k+1, 3) + 1
                do l = 1, 3
                    do m = 1, 3
                        do n = 1, 2
                            do o = 1, 2
                                do p = 1, 2
                                    userHeaderStr = ' '
                                    unitSpec = unit(l)
                                    count = count + 1
                                    ! write(*,*) i, j, k, l, m, n, o, p
                                    ifltab = 0
                                    if (i == 1) then
                                        call zopen6(ifltab, filename(i), status)
                                    else
                                        call zopen7(ifltab, filename(i), status)
                                    end if
                                    call assert(status == 0)
                                    !--------------------------------!
                                    ! set the default vertical datum !
                                    !--------------------------------!
                                    kk = k
                                    call zset('VDTM', verticalDatums(kk), 0)
                                    if (p == 1) then
                                        !------------------------------------------------!
                                        ! add the vertical datum info to the user header !
                                        !------------------------------------------------!
                                        userHeaderStr = VERTICAL_DATUM_INFO_PARAM//':'//vdiStr//';'
                                    end if
                                    if (m > 1) then
                                        !----------------------------------------------------------!
                                        ! override the default vertical datum with the user header !
                                        !----------------------------------------------------------!
                                        kk = k2
                                        len = len_trim(userHeaderStr) + 1
                                        write(userHeaderStr(len:), '(3a)') &
                                            VERTICAL_DATUM_PARAM,':',verticalDatums(kk)
                                    end if
                                    if (m > 2) then
                                        !--------------------------------------------------------!
                                        ! override default and user header datums with unit spec !
                                        !--------------------------------------------------------!
                                        kk = k3
                                        write(unitSpec,'(5a)')                     &
                                            'U=',unit(l)(1:len_trim(unit(l))),'|', &
                                            'V=',verticalDatums(kk)
                                    end if
                                    !-----------------------------------------------!
                                    ! figure out whether the zs?tsx? should succeed !
                                    !-----------------------------------------------!
                                    if (i==2.and.j==2.and.k==1.and.l==1.and.m==1.and.n==1.and.o==1.and.p==1) then
                                        !-------------------------------------------------------------------------------!
                                        ! change of vertical datum information in DSS 7, need to update location record !
                                        !-------------------------------------------------------------------------------!
                                        expectSuccess = .false.
                                    elseif (i==1.and.p==2.and.len_trim(userHeaderStr).gt.0) then
                                        !---------------------------------------------------------------------------------!
                                        ! current vertical datum in header, but no vertical datum info in header in DSS 6 !
                                        !---------------------------------------------------------------------------------!
                                        expectSuccess = .false.
                                    elseif (vdi(j)%nativeDatum == verticalDatums(kk)) then
                                        !-------------------------------------!
                                        ! same datum, no conversion necessary !
                                        !-------------------------------------!
                                        expectSuccess = .true.
                                    else if (verticalDatums(kk) /= CVD_NAVD88.and.verticalDatums(kk) /= CVD_NGVD29) then
                                        !--------------------------!
                                        ! requested datum is local !
                                        !--------------------------!
                                        if (vdi(j)%nativeDatum == CVD_NAVD88.or.vdi(j)%nativeDatum == CVD_NGVD29) then
                                            !---------------------------!
                                            ! native datum is non-local !
                                            !---------------------------!
                                            expectSuccess = .false.
                                        else
                                            !-----------------------!
                                            ! native datum is local !
                                            !-----------------------!
                                            expectSuccess = .true.
                                        end if
                                    else if (verticalDatums(kk) == CVD_NAVD88 .and. &
                                            vdi(j)%offsetToNavd88 /= UNDEFINED_VERTICAL_DATUM_VALUE) then
                                        !-------------------------------------------------------------!
                                        ! specified datum is NAVD-88 and we have an offset to NAVD-88 !
                                        !-------------------------------------------------------------!
                                        if (unitIsFeet(unit(l)).or.unitIsMeters(unit(l))) then
                                            expectSuccess = .true.
                                        else
                                            expectSuccess = .false.
                                        end if
                                    else if (verticalDatums(kk) == CVD_NGVD29 .and. &
                                            vdi(j)%offsetToNgvd29 /= UNDEFINED_VERTICAL_DATUM_VALUE) then
                                        !-------------------------------------------------------------!
                                        ! specified datum is NGVD-29 and we have an offset to NGVD-29 !
                                        !-------------------------------------------------------------!
                                        if (unitIsFeet(unit(l)).or.unitIsMeters(unit(l))) then
                                            expectSuccess = .true.
                                        else
                                            expectSuccess = .false.
                                        end if
                                    else
                                        !-----------------!
                                        ! all other cases !
                                        !-----------------!
                                        expectSuccess = .false.
                                    end if
                                    if (expectSuccess) then
                                        write(*,'(a,i3,a)') 'Time series test ',count,' expecting SUCCESS'
                                    else
                                        write(*,'(a,i3,a)') 'Time seires test ',count,' expecting ERROR'
                                    end if
                                    !-------------------------------------------------------!
                                    ! store the time series in the specified vertical datum !
                                    !-------------------------------------------------------!
                                    userHeaderLen = byteCountToIntCount(len_trim(userHeaderStr))
									if (isBigEndian()) then
										do ii = 1, userHeaderLen
											userHeader(ii) = iswap(userHeader(ii))
										end do
									end if
                                    numberValues = 6
                                    if (n == 1) then
                                        if (o == 1) then
                                            !-------------!
                                            ! RTS doubles !
                                            !-------------!
                                            dvals(1:numberValues) = dvalues(:,l)
                                            call zsrtsxd(               &
                                                ifltab,                 & ! IFLTAB
                                                pathnames(o,n),         & ! CPATH
                                                startDate,              & ! CDATE
                                                startTime,              & ! CTIME
                                                numberValues,           & ! NVALS
                                                dvals,                  & ! DVALUES
                                                quality,                & ! JQUAL
                                                .true.,                 & ! LQUAL
                                                unitSpec,               & ! CUNITS
                                                type,                   & ! CTYPE
                                                userHeader,             & ! IUHEAD
                                                userHeaderLen,          & ! NUHEAD
                                                0,                      & ! IPLAN
                                                0,0.,.false.,.false.,0, & ! Compression control
                                                status)
                                        else
                                            !------------!
                                            ! RTS floats !
                                            !------------!
                                            fvals(1:numberValues) = fvalues(:,l)
                                            call zsrtsx(                &
                                                ifltab,                 & ! IFLTAB
                                                pathnames(o,n),         & ! CPATH
                                                startDate,              & ! CDATE
                                                startTime,              & ! CTIME
                                                numberValues,           & ! NVALS
                                                fvals,                  & ! VALUES
                                                quality,                & ! JQUAL
                                                .true.,                 & ! LQUAL
                                                unitSpec,               & ! CUNITS
                                                type,                   & ! CTYPE
                                                userHeader,             & ! IUHEAD
                                                userHeaderLen,          & ! NUHEAD
                                                0,                      & ! IPLAN
                                                0,0.,.false.,.false.,0, & ! Compression control
                                                status)
                                        end if
                                    else
                                        if (o == 1) then
                                            !-------------!
                                            ! ITS doubles !
                                            !-------------!
                                            dvals(1:numberValues) = dvalues(:,l)
                                            call zsitsxd(               &
                                                ifltab,                 & ! IFLTAB
                                                pathnames(o,n),         & ! CPATH
                                                itimes,                 & ! ITIMES
                                                dvals,                  & ! DVALUES
                                                numberValues,           & ! NVALUE
                                                0,                      & ! IBDATE
                                                quality,                & ! JQUAL
                                                .true.,                 & ! LSQUAL
                                                unitSpec,               & ! CUNITS
                                                type,                   & ! CTYPE
                                                userHeader,             & ! IHEADU
                                                userHeaderLen,          & ! NHEADU
                                                1,                      & ! INFLAG
                                                status)
                                        else
                                            !------------!
                                            ! ITS floats !
                                            !------------!
                                            ! write(0,*) 'Calling zsitx'
                                            fvals(1:numberValues) = fvalues(:,l)
                                            call zsitsx(                &
                                                ifltab,                 & ! IFLTAB
                                                pathnames(o,n),         & ! CPATH
                                                itimes     ,            & ! ITIMES
                                                fvals,                  & ! VALUES
                                                numberValues,           & ! NVALUE
                                                0,                      & ! IBDATE
                                                quality,                & ! JQUAL
                                                .true.,                 & ! LSQUAL
                                                unitSpec,               & ! CUNITS
                                                type,                   & ! CTYPE
                                                userHeader,             & ! IHEADU
                                                userHeaderLen,          & ! NHEADU
                                                1,                      & ! INFLAG
                                                status)
                                        end if
                                    end if
                                    call assert((status == 0) .eqv. expectSuccess)
                                    if (i==2.and.j==2.and.k==1.and.l==1.and.m==1.and.n==1.and.o==1) then
                                        !-------------------------------------------------------------------------------!
                                        ! change of vertical datum information in DSS 7, need to update location record !
                                        !-------------------------------------------------------------------------------!
                                        call zset('VDOW', ' ', 1)
                                        count = count + 1
                                        write(0,'(a,i3,a)') 'Time series test ',count,' expecting SUCCESS'
                                        if (n == 1) then
                                            if (o == 1) then
                                                !-------------!
                                                ! RTS doubles !
                                                !-------------!
                                                call zsrtsxd(               &
                                                    ifltab,                 & ! IFLTAB
                                                    pathnames(o,n),         & ! CPATH
                                                    startDate,              & ! CDATE
                                                    startTime,              & ! CTIME
                                                    numberValues,           & ! NVALS
                                                    dvals,                  & ! DVALUES
                                                    quality,                & ! JQUAL
                                                    .true.,                 & ! LQUAL
                                                    unitSpec,               & ! CUNITS
                                                    type,                   & ! CTYPE
                                                    userHeader,             & ! IUHEAD
                                                    userHeaderLen,          & ! NUHEAD
                                                    0,                      & ! IPLAN
                                                    0,0.,.false.,.false.,0, & ! Compression control
                                                    status)
                                            else
                                                !------------!
                                                ! RTS floats !
                                                !------------!
                                                call zsrtsx(                &
                                                    ifltab,                 & ! IFLTAB
                                                    pathnames(o,n),         & ! CPATH
                                                    startDate,              & ! CDATE
                                                    startTime,              & ! CTIME
                                                    numberValues,           & ! NVALS
                                                    fvals,                  & ! VALUES
                                                    quality,                & ! JQUAL
                                                    .true.,                 & ! LQUAL
                                                    unitSpec,               & ! CUNITS
                                                    type,                   & ! CTYPE
                                                    userHeader,             & ! IUHEAD
                                                    userHeaderLen,          & ! NUHEAD
                                                    0,                      & ! IPLAN
                                                    0,0.,.false.,.false.,0, & ! Compression control
                                                    status)
                                            end if
                                        else
                                            if (o == 1) then
                                                !-------------!
                                                ! ITS doubles !
                                                !-------------!
                                                call zsitsxd(               &
                                                    ifltab,                 & ! IFLTAB
                                                    pathnames(o,n),         & ! CPATH
                                                    itimes,                 & ! ITIMES
                                                    dvals,                  & ! DVALUES
                                                    numberValues,           & ! NVALUE
                                                    0,                      & ! IBDATE
                                                    quality,                & ! JQUAL
                                                    .true.,                 & ! LSQUAL
                                                    unitSpec,               & ! CUNITS
                                                    type,                   & ! CTYPE
                                                    userHeader,             & ! IHEADU
                                                    userHeaderLen,          & ! NHEADU
                                                    1,                      & ! INFLAG
                                                    status)
                                            else
                                                !------------!
                                                ! ITS floats !
                                                !------------!
                                                call zsitsx(                &
                                                    ifltab,                 & ! IFLTAB
                                                    pathnames(o,n),         & ! CPATH
                                                    itimes,                 & ! ITIMES
                                                    fvals,                  & ! VALUES
                                                    numberValues,           & ! NVALUE
                                                    0,                      & ! IBDATE
                                                    quality,                & ! JQUAL
                                                    .true.,                 & ! LSQUAL
                                                    unitSpec,               & ! CUNITS
                                                    type,                   & ! CTYPE
                                                    userHeader,             & ! IHEADU
                                                    userHeaderLen,          & ! NHEADU
                                                    1,                      & ! INFLAG
                                                    status)
                                            end if
                                        end if
                                        call zset('VDOW', ' ', 0)
                                        call assert(status == 0)
                                    end if
                                    call zclose(ifltab)
                                    if (status == 0) then
                                        !------------------------------------------------------------!
                                        ! set the default vertical datum to the datum we stored with !
                                        !------------------------------------------------------------!
                                        call zset('VDTM', verticalDatums(kk), 0)
                                        !--------------------------------------------------------!
                                        ! retrieve the time series in the default vertical datum !
                                        !--------------------------------------------------------!
                                        ifltab = 0
                                        if (i == 1) then
                                            call zopen6(ifltab, filename(i), status)
                                        else
                                            call zopen7(ifltab, filename(i), status)
                                        end if
                                        call assert(status == 0)
                                        if (n == 1) then
                                            if (o == 1) then
                                                !-------------!
                                                ! RTS doubles !
                                                !-------------!
                                                dvals(1:numberValues) = dvalues(:,l)
                                                call zrrtsxd(          &
                                                    ifltab,            & ! IFLTAB
                                                    pathnames(o,n),    & ! CPATH
                                                    startDate,         & ! CDATE
                                                    startTime,         & ! CTIME
                                                    numberValues,      & ! NVALS
                                                    dvals_out,         & ! DVALS
                                                    quality,           & ! JQUAL
                                                    readQuality,       & ! LQUAL
                                                    qualityWasRead,    & ! LQREAD
                                                    unitSpec,          & ! CUNITS
                                                    type,              & ! CTYPE
                                                    userHeader,        & ! IUHEAD
                                                    size(userHeader),  & ! KUHEAD
                                                    userHeaderLen,     & ! NUHEAD
                                                    intervalOffset,    & ! IOFSET
                                                    compressionMethod, & ! JCOMP
                                                    status)              ! ISTAT
                                            else
                                                !------------!
                                                ! RTS floats !
                                                !------------!
                                                fvals(1:numberValues) = fvalues(:,l)
                                                call zrrtsx(           &
                                                    ifltab,            & ! IFLTAB
                                                    pathnames(o,n),    & ! CPATH
                                                    startDate,         & ! CDATE
                                                    startTime,         & ! CTIME
                                                    numberValues,      & ! NVALS
                                                    fvals_out,         & ! DVALS
                                                    quality,           & ! JQUAL
                                                    readQuality,       & ! LQUAL
                                                    qualityWasRead,    & ! LQREAD
                                                    unitSpec,          & ! CUNITS
                                                    type,              & ! CTYPE
                                                    userHeader,        & ! IUHEAD
                                                    size(userHeader),  & ! KUHEAD
                                                    userHeaderLen,     & ! NUHEAD
                                                    intervalOffset,    & ! IOFSET
                                                    compressionMethod, & ! JCOMP
                                                    status)              ! ISTAT
                                            end if
                                        else
                                            call datjul(startDate, startDay, status)
                                            call assert(status == 0)
                                            call datjul(endDate, endDay, status)
                                            call assert(status == 0)
                                            if (o == 1) then
                                                !-------------!
                                                ! ITS doubles !
                                                !-------------!
                                                dvals(1:numberValues) = dvalues(:,l)
                                                call zritsxd(          &
                                                    ifltab,            & ! IFLTAB  in/out
                                                    pathnames(o,n),    & ! CPATH   in
                                                    startDay,          & ! JULS    in
                                                    ihm2m(startTime),    & ! ISTIME  in
                                                    endDay,            & ! JULE    in
                                                    ihm2m(endTime),      & ! IETIME  in
                                                    timesRetrieved,    & ! ITIMES  out
                                                    dvals_out,         & ! DVALUES out
                                                    size(dvals_out),   & ! KVALS   in
                                                    numberValues,      & ! NVALS   out
                                                    baseDate,          & ! IBDATE  out
                                                    quality,           & ! IQUAL   out
                                                    readQuality,       & ! LQUAL   in
                                                    qualityWasRead,    & ! LQREAD  out
                                                    unitSpec,          & ! CUNITS  out
                                                    type,              & ! CTYPE   out
                                                    userHeader,        & ! IUHEAD  out
                                                    size(userHeader),  & ! KUHEAD  in
                                                    userHeaderLen,     & ! NUHEAD  out
                                                    0,                 & ! INFLAG  in
                                                    status)              ! ISTAT   out
                                            else
                                                !------------!
                                                ! ITS floats !
                                                !------------!
                                                fvals(1:numberValues) = fvalues(:,l)
                                                call zritsx(           &
                                                    ifltab,            & ! IFLTAB
                                                    pathnames(o,n),    & ! CPATH
                                                    startDay,          & ! JULS
                                                    ihm2m(startTime),    & ! ISTIME
                                                    endDay,            & ! JULE
                                                    ihm2m(endTime),      & ! IETIME
                                                    timesRetrieved,    & ! ITIMES
                                                    fvals_out,         & ! SVALUES
                                                    size(fvals_out),   & ! KVALS
                                                    numberValues,      & ! NVALS
                                                    baseDate,          & ! IBDATE
                                                    quality,           & ! IQUAL
                                                    readQuality,       & ! LQUAL
                                                    qualityWasRead,    & ! LQREAD
                                                    unitSpec,          & ! CUNITS
                                                    type,              & ! CTYPE
                                                    userHeader,        & ! IUHEAD
                                                    size(userHeader),  & ! KUHEAD
                                                    userHeaderLen,     & ! NUHEAD
                                                    0,                 & ! INFLAG
                                                    status)              ! ISTAT
                                            end if
                                        end if
                                        call zclose(ifltab)
                                        call assert(status == 0)
                                        !------------------------------------------------------!
                                        ! compare the retrieved time seires to what was stored !
                                        !------------------------------------------------------!
                                        call assert(numberValues == 6)
                                        if (o == 1) then
                                            do ii = 1, numberValues
                                            end do
                                            do ii = 1, numberValues
                                                call assert(dvals_out(i) == dvals(i))
                                            end do
                                        else
                                            do ii = 1, numberValues
                                                call assert(fvals_out(i) == fvals(i))
                                            end do
                                        end if
                                    end if
                                end do
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end do
    write(0,'(/,/,i3,a,/,/)') count,' time sereies tests passed'
    return
end subroutine testStoreRetrieveTimeSeries

subroutine testStoreRetrievePairedData()
    use modVerticalDatumInfo
    implicit none

    integer (kind=8)        :: ifltab(250)
    integer (kind=4)        :: status, numberOrdinates, numberCurves, ihoriz, nvals, count, iVerticalDatum
    integer (kind=4)        :: userHeader(100), userHeaderLen, i, j, k, k2, k3, kk, l, m, n, o, p, ii, len
    real (kind=8)           :: dordinates(6,3), dvalues(6,3), dvals(12), dvals_out(12)
    real (kind=4)           :: fordinates(6,3), fvalues(6,3), fvals(12), fvals_out(12)
    character (len=300)     :: errmsg, vdiStr
    character (len=80)      :: filename(2)
    character (len=80)      :: pathnames(2,2)
    character (len=16)      :: unit(3), type, verticalDatums(3), c1unit, c2unit, c1type, c2type, clabel, cVerticalDatum
    character (len=32)      :: unitSpec
    character (len=4)       :: startTime, endTime
    character (len=400)     :: userHeaderStr
    logical                 :: l_label, expectSuccess
    type(verticalDatumInfo) :: vdi(2)


    equivalence (userHeader, userHeaderStr)

    ifltab = 0
    numberOrdinates = 6
    numberCurves = 1
    userHeader = 0
    dvals = 0.
    fvals = 0.

    dordinates = reshape((/                                &
        1000.,1001.,1002.,1003.,1004.,1005.,               & ! ft
        304.8,305.1048,305.4096,305.7144,306.0192,306.324, & ! m
        1000.,1001.,1002.,1003.,1004.,1005.                & ! cfs
        /),shape(dvalues))

    dvalues = dordinates

    fordinates = dordinates

    fvalues = fordinates

    filename = (/'v6_f.dss', 'v7_f.dss'/)

    pathnames = reshape((/                    &
        '//TestPdLoc/Stage-Elev///Doubles/ ', &
        '//TestPdLoc/Stage-Elev///Floats/  ', &
        '//TestPdLoc/Elev-Stage///Doubles/ ', &
        '//TestPdLoc/Elev-Stage///Floats/  '  &
        /), shape(pathnames))


    unit = (/'ft ', 'm  ', 'cfs'/)

    type = 'UNT'

    verticalDatums = (/CVD_NAVD88, CVD_NGVD29, 'Pensacola       '/)

    do j = 1, 2
        call initVerticalDatumInfo(vdi(j))
        if (j == 1) then
            vdi(j)%nativeDatum = CVD_NGVD29
            vdi(j)%unit = 'ft'
            vdi(j)%offsetToNavd88 = 0.3855
            vdi(j)%offsetToNavd88IsEstimate = .true.
        else
            vdi(j)%nativeDatum = 'Pensacola'
            vdi(j)%unit = 'ft'
            vdi(j)%offsetToNavd88 = 1.457
            vdi(j)%offsetToNavd88IsEstimate = .true.
            vdi(j)%offsetToNgvd29 = 1.07
            vdi(j)%offsetToNgvd29IsEstimate = .false.
        end if
    end do
    !
    ! loop variables
    !
    ! i = DSS file version
    !     1 = DSS 6
    !     2 = DSS 7
    !
    ! j = vdi
    !     1 = NGVD-29 native
    !     2 = OTHER native with local datum named "Pensacola"
    !
    ! k = vertical datum
    !     1 = NAVD-88
    !     2 = NGVD-29
    !     3 = OTHER (Pensacola)
    !     k2  mod(k, 3) + 1
    !     k3  mod(k+1, 3) + 1
    !
    ! l = data units
    !     1 = ft
    !     2 = m
    !     3 = cfs (invalid for datum conversion)
    !
    ! m = vertical datum specification method
    !     1 = set default with zset
    !     2 = 1 plus override with user header
    !     3 = 2 plus override with unit spec
    !
    ! n = elevation param position
    !     1 = dependent parameter
    !     2 = independent parameter
    !
    ! o = data value type
    !     1 = doubles
    !     2 = floats
    !
    ! p = specify vertical datum info in user header on store
    !     1 = specify
    !     2 = don't specify (use previously stored)
    !
    call zset('MLVL', '', 1)
    count = 0
    do i = 1, 2
        call deletefile(filename(i), status)
        do j = 1, 2
            call verticalDatumInfoToString(       &
                vdiStr,                           &
                errmsg,                           &
                vdi(j)%nativeDatum,               &
                vdi(j)%unit,                      &
                vdi(j)%offsetToNgvd29,            &
                vdi(j)%offsetToNgvd29IsEstimate,  &
                vdi(j)%offsetToNavd88,            &
                vdi(j)%offsetToNavd88IsEstimate,  &
                .true.)
            call assert(errmsg == ' ')
            call stringToVerticalDatumInfo(      &
                vdiStr,                          &
                errmsg,                          &
                vdi(j)%nativeDatum,              &
                vdi(j)%unit,                     &
                vdi(j)%offsetToNgvd29,           &
                vdi(j)%offsetToNgvd29IsEstimate, &
                vdi(j)%offsetToNavd88,           &
                vdi(j)%offsetToNavd88IsEstimate)
            call assert(errmsg == ' ')
            do k = 1, 3
                k2 = mod(k, 3) + 1
                k3 = mod(k+1, 3) + 1
                do l = 1, 3
                    do m = 1, 3
                        do n = 1, 2
                            do o = 1, 2
                                do p = 1, 2
                                    userHeaderStr = ' '
                                    unitSpec = unit(l)
                                    count = count + 1
                                    ! write(0,*) i, j, k, l, m, n, o, p
                                    ifltab = 0
                                    if (i == 1) then
                                        call zopen6(ifltab, filename(i), status)
                                    else
                                        call zopen7(ifltab, filename(i), status)
                                    end if
                                    call assert(status == 0)
                                    !--------------------------------!
                                    ! set the default vertical datum !
                                    !--------------------------------!
                                    kk = k
                                    call zset('VDTM', verticalDatums(kk), 0)
                                    if (p == 1) then
                                      !------------------------------------------------!
                                      ! add the vertical datum info to the user header !
                                      !------------------------------------------------!
                                      userHeaderStr = VERTICAL_DATUM_INFO_PARAM//':'//vdiStr
                                    end if
                                    if (m > 1) then
                                        !----------------------------------------------------------!
                                        ! override the default vertical datum with the user header !
                                        !----------------------------------------------------------!
                                        kk = k2
                                        if (userHeaderStr /= ' ') then
                                            len = len_trim(userHeaderStr) + 1
                                            userHeaderStr(len:) = ';'
                                        end if
                                        len = len_trim(userHeaderStr) + 1
                                        write(userHeaderStr(len:), '(3a)') &
                                            VERTICAL_DATUM_PARAM,':',verticalDatums(kk)
                                    end if
                                    if (m > 2) then
                                        !--------------------------------------------------------!
                                        ! override default and user header datums with unit spec !
                                        !--------------------------------------------------------!
                                        kk = k3
                                        write(unitSpec,'(5a)')                     &
                                            'U=',unit(l)(1:len_trim(unit(l))),'|', &
                                            'V=',verticalDatums(kk)
                                    end if
                                    !--------------------------------------------------------------------------!
                                    ! figure out whether the zspd? should have succeeded, and test accordingly !
                                    !--------------------------------------------------------------------------!
                                    if (i==2.and.j==2.and.k==1.and.l==1.and.m==1.and.n==1.and.o==1.and.p==1) then
                                        !-------------------------------------------------------------------------------!
                                        ! change of vertical datum information in DSS 7, need to update location record !
                                        !-------------------------------------------------------------------------------!
                                        expectSuccess = .false.
                                    elseif (i==1.and.p==2.and.len_trim(userHeaderStr).gt.0) then
                                        !---------------------------------------------------------------------------------!
                                        ! current vertical datum in header, but no vertical datum info in header in DSS 6 !
                                        !---------------------------------------------------------------------------------!
                                        expectSuccess = .false.
                                    else if (vdi(j)%nativeDatum == verticalDatums(kk)) then
                                        !-------------------------------------!
                                        ! same datum, no conversion necessary !
                                        !-------------------------------------!
                                        expectSuccess = .true.
                                    elseif (i==1.and.p==2.and.index(userHeaderStr, vdiStr).gt.0) then
                                        !---------------------------------------------------------------------------------!
                                        ! current vertical datum in header, but no vertical datum info in header in DSS 6 !
                                        !---------------------------------------------------------------------------------!
                                        expectSuccess = .false.
                                    elseif (verticalDatums(kk) /= CVD_NAVD88 .and. verticalDatums(kk) /= CVD_NGVD29) then
                                        !--------------------------!
                                        ! requested datum is local !
                                        !--------------------------!
                                        if (vdi(j)%nativeDatum == CVD_NAVD88 .or. vdi(j)%nativeDatum == CVD_NGVD29) then
                                            !---------------------------!
                                            ! native datum is non-local !
                                            !---------------------------!
                                            expectSuccess = .false.
                                        else
                                            !-----------------------!
                                            ! native datum is local !
                                            !-----------------------!
                                            expectSuccess = .true.
                                        end if
                                    elseif (verticalDatums(kk) == CVD_NAVD88 .and.  &
                                            vdi(j)%offsetToNavd88 /= UNDEFINED_VERTICAL_DATUM_VALUE) then
                                        !-------------------------------------------------------------!
                                        ! specified datum is NAVD-88 and we have an offset to NAVD-88 !
                                        !-------------------------------------------------------------!
                                        if (unitIsFeet(unit(l)).or.unitIsMeters(unit(l))) then
                                            expectSuccess = .true.
                                        else
                                            expectSuccess = .false.
                                        end if
                                    elseif (verticalDatums(kk) == CVD_NGVD29 .and.  &
                                            vdi(j)%offsetToNgvd29 /= UNDEFINED_VERTICAL_DATUM_VALUE) then
                                        !-------------------------------------------------------------!
                                        ! specified datum is NGVD-29 and we have an offset to NGVD-29 !
                                        !-------------------------------------------------------------!
                                        if (unitIsFeet(unit(l)).or.unitIsMeters(unit(l))) then
                                            expectSuccess = .true.
                                        else
                                            expectSuccess = .false.
                                        end if
                                    else
                                        !-----------------!
                                        ! all other cases !
                                        !-----------------!
                                        expectSuccess = .false.
                                    end if
                                    if (expectSuccess) then
                                        write(0,'(a,i3,a)') 'Paired data test ',count,' expecting SUCCESS'
                                    else
                                        write(0,'(a,i3,a)') 'Paired data test ',count,' expecting ERROR'
                                    end if
                                    !-------------------------------------------------------!
                                    ! store the paried data in the specified vertical datum !
                                    !-------------------------------------------------------!
                                    userHeaderLen = byteCountToIntCount(len_trim(userHeaderStr))
									if (isBigEndian()) then
										do ii = 1, userHeaderLen
											userHeader(ii) = iswap(userHeader(ii))
										end do
									end if
                                    if (o == 1) then
                                        !---------!
                                        ! doubles !
                                        !---------!
                                        dvals(1:6) = dordinates(:,l)
                                        dvals(7:12) = dvalues(:,l)
                                        if (n == 1) then
                                            call zspdd(          & !
                                                ifltab,          & ! IFLTAB
                                                pathnames(o,n),  & ! CPATH
                                                numberOrdinates, & ! NORD
                                                numberCurves,    & ! NCURVE
                                                1,               & ! IHORIZ
                                                unit(l),         & ! C1UNIT
                                                type,            & ! C1TYPE
                                                unitSpec,        & ! C2UNIT
                                                type,            & ! C2TYPE
                                                dvals,           & ! DVALUES
                                                '',              & ! CLABEL
                                                .false.,         & ! LABEL
                                                userHeader,      & ! IUHEAD
                                                userHeaderLen,   & ! NUHEAD
                                                0,               & ! IPLAN
                                                status)            ! ISTAT
                                        else
                                            call zspdd(          & !
                                                ifltab,          & ! IFLTAB
                                                pathnames(o,n),  & ! CPATH
                                                numberOrdinates, & ! NORD
                                                numberCurves,    & ! NCURVE
                                                1,               & ! IHORIZ
                                                unitSpec,        & ! C1UNIT
                                                type,            & ! C1TYPE
                                                unit(l),         & ! C2UNIT
                                                type,            & ! C2TYPE
                                                dvals,           & ! DVALUES
                                                '',              & ! CLABEL
                                                .false.,         & ! LABEL
                                                userHeader,      & ! IUHEAD
                                                userHeaderLen,   & ! NUHEAD
                                                0,               & ! IPLAN
                                                status)            ! ISTAT
                                        end if
                                    else
                                        !--------!
                                        ! floats !
                                        !--------!
                                        fvals(1:6) = fordinates(:,l)
                                        fvals(7:12) = fvalues(:,l)
                                        if (n == 1) then
                                            call zspd(           & !
                                                ifltab,          & ! IFLTAB
                                                pathnames(o,n),  & ! CPATH
                                                numberOrdinates, & ! NORD
                                                numberCurves,    & ! NCURVE
                                                1,               & ! IHORIZ
                                                unit(l),         & ! C1UNIT
                                                type,            & ! C1TYPE
                                                unitSpec,        & ! C2UNIT
                                                type,            & ! C2TYPE
                                                fvals,           & ! SVALUES
                                                '',              & ! CLABEL
                                                .false.,         & ! LABEL
                                                userHeader,      & ! IUHEAD
                                                userHeaderLen,   & ! NUHEAD
                                                0,               & ! IPLAN
                                                status)            ! ISTAT
                                        else
                                            call zspd(           & !
                                                ifltab,          & ! IFLTAB
                                                pathnames(o,n),  & ! CPATH
                                                numberOrdinates, & ! NORD
                                                numberCurves,    & ! NCURVE
                                                1,               & ! IHORIZ
                                                unitSpec,        & ! C1UNIT
                                                type,            & ! C1TYPE
                                                unit(l),         & ! C2UNIT
                                                type,            & ! C2TYPE
                                                fvals,           & ! SVALUES
                                                '',              & ! CLABEL
                                                .false.,         & ! LABEL
                                                userHeader,      & ! IUHEAD
                                                userHeaderLen,   & ! NUHEAD
                                                0,               & ! IPLAN
                                                status)            ! ISTAT
                                        end if
                                    end if
                                    call assert((status == 0) .eqv. expectSuccess)
                                    if (i==2.and.j==2.and.k==1.and.l==1.and.m==1.and.n==1.and.o==1) then
                                        count = count + 1
                                        write(0,'(a,i3,a)') 'Paired data test ',count,' expecting SUCCESS'
                                        call zset('VDOW', '', 1)
                                        if (o == 1) then
                                            !---------!
                                            ! doubles !
                                            !---------!
                                            if (n == 1) then
                                                call zspdd(          & !
                                                    ifltab,          & ! IFLTAB
                                                    pathnames(o,n),  & ! CPATH
                                                    numberOrdinates, & ! NORD
                                                    numberCurves,    & ! NCURVE
                                                    1,               & ! IHORIZ
                                                    unit(l),         & ! C1UNIT
                                                    type,            & ! C1TYPE
                                                    unitSpec,        & ! C2UNIT
                                                    type,            & ! C2TYPE
                                                    dvals,           & ! DVALUES
                                                    '',              & ! CLABEL
                                                    .false.,         & ! LABEL
                                                    userHeader,      & ! IUHEAD
                                                    userHeaderLen,   & ! NUHEAD
                                                    0,               & ! IPLAN
                                                    status)            ! ISTAT
                                            else
                                                call zspdd(          & !
                                                    ifltab,          & ! IFLTAB
                                                    pathnames(o,n),  & ! CPATH
                                                    numberOrdinates, & ! NORD
                                                    numberCurves,    & ! NCURVE
                                                    1,               & ! IHORIZ
                                                    unitSpec,        & ! C1UNIT
                                                    type,            & ! C1TYPE
                                                    unit(l),         & ! C2UNIT
                                                    type,            & ! C2TYPE
                                                    dvals,           & ! DVALUES
                                                    '',              & ! CLABEL
                                                    .false.,         & ! LABEL
                                                    userHeader,      & ! IUHEAD
                                                    userHeaderLen,   & ! NUHEAD
                                                    0,               & ! IPLAN
                                                    status)            ! ISTAT
                                            end if
                                        else
                                            !--------!
                                            ! floats !
                                            !--------!
                                            if (n == 1) then
                                                call zspd(           & !
                                                    ifltab,          & ! IFLTAB
                                                    pathnames(o,n),  & ! CPATH
                                                    numberOrdinates, & ! NORD
                                                    numberCurves,    & ! NCURVE
                                                    1,               & ! IHORIZ
                                                    unit(l),         & ! C1UNIT
                                                    type,            & ! C1TYPE
                                                    unitSpec,        & ! C2UNIT
                                                    type,            & ! C2TYPE
                                                    fvals,           & ! SVALUES
                                                    '',              & ! CLABEL
                                                    .false.,         & ! LABEL
                                                    userHeader,      & ! IUHEAD
                                                    userHeaderLen,   & ! NUHEAD
                                                    0,               & ! IPLAN
                                                    status)            ! ISTAT
                                            else
                                                call zspd(           & !
                                                    ifltab,          & ! IFLTAB
                                                    pathnames(o,n),  & ! CPATH
                                                    numberOrdinates, & ! NORD
                                                    numberCurves,    & ! NCURVE
                                                    1,               & ! IHORIZ
                                                    unitSpec,        & ! C1UNIT
                                                    type,            & ! C1TYPE
                                                    unit(l),         & ! C2UNIT
                                                    type,            & ! C2TYPE
                                                    fvals,           & ! SVALUES
                                                    '',              & ! CLABEL
                                                    .false.,         & ! LABEL
                                                    userHeader,      & ! IUHEAD
                                                    userHeaderLen,   & ! NUHEAD
                                                    0,               & ! IPLAN
                                                    status)            ! ISTAT
                                            end if
                                        end if
                                        call assert(status == 0)
                                        call zset('VDOW', '', 0)
                                    end if
                                    call zclose(ifltab)
                                    if (status == 0) then
                                        !------------------------------------------------------------!
                                        ! set the default vertical datum to the datum we stored with !
                                        !------------------------------------------------------------!
                                        call zset('VDTM', verticalDatums(kk), 0)
                                      !--------------------------------------------------------!
                                      ! retrieve the paired data in the default vertical datum !
                                      !--------------------------------------------------------!
                                        ifltab = 0
                                        if (i == 1) then
                                            call zopen6(ifltab, filename(i), status)
                                        else
                                            call zopen7(ifltab, filename(i), status)
                                        end if
                                        call assert(status == 0)
                                        if (o == 1) then
                                            !---------!
                                            ! doubles !
                                            !---------!
                                            dvals(1:6) = dordinates(:,l)
                                            dvals(7:12) = dvalues(:,l)
                                            call zrpdd(           &
                                                ifltab,           & ! IFLTAB
                                                pathnames(o,n),   & ! CPATH
                                                numberOrdinates,  & ! NORD
                                                numberCurves,     & ! NCURVE
                                                ihoriz,           & ! IHORIZ
                                                c1unit,           & ! C1UNIT
                                                c1type,           & ! C1TYPE
                                                c2unit,           & ! C2UNIT
                                                c2type,           & ! C2TYPE
                                                dvals_out,        & ! DVALUES
                                                size(dvals),      & ! KVALS
                                                nvals,            & ! NVALS
                                                clabel,           & ! CLABEL
                                                0,                & ! KLABEL
                                                l_label,          & ! LABEL
                                                userHeader,       & ! IUHEAD
                                                size(userHeader), & ! KUHEAD
                                                userHeaderLen,    & ! NUHEAD
                                                status)             ! ISTAT
                                        else
                                            !--------!
                                            ! floats !
                                            !--------!
                                            fvals(1:6) = fordinates(:,l)
                                            fvals(7:12) = fvalues(:,l)
                                            call zrpd(            &
                                                ifltab,           & ! IFLTAB
                                                pathnames(o,n),   & ! CPATH
                                                numberOrdinates,  & ! NORD
                                                numberCurves,     & ! NCURVE
                                                ihoriz,           & ! IHORIZ
                                                c1unit,           & ! C1UNIT
                                                c1type,           & ! C1TYPE
                                                c2unit,           & ! C2UNIT
                                                c2type,           & ! C2TYPE
                                                fvals_out,        & ! SVALUES
                                                size(fvals),      & ! KVALS
                                                nvals,            & ! NVALS
                                                clabel,           & ! CLABEL
                                                0,                & ! KLABEL
                                                l_label,          & ! LABEL
                                                userHeader,       & ! IUHEAD
                                                size(userHeader), & ! KUHEAD
                                                userHeaderLen,    & ! NUHEAD
                                                status)             ! ISTAT
                                        end if
										call zclose(ifltab)
                                        call assert(status == 0)
                                        call assert(numberOrdinates == 6)
                                        call assert(numberCurves == 1)
                                        call assert(c1unit == unit(l))
                                        call assert(c2unit == unit(l))
                                        call assert(c1type == type)
                                        call assert(c2type == type)
                                        if (o == 1) then
                                            call assert(nvals == size(dvals))
                                            do ii = 1, nvals
                                                call assert(dvals_out(ii) == dvals(ii))
                                            end do
                                        else
                                            call assert(nvals == size(fvals))
                                            do ii = 1, nvals
                                                call assert(fvals_out(ii) == fvals(ii))
                                            end do
                                        end if
                                    end if
                                end do
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end do
    write(0,'(/,/,i3,a/,/)') count,' paried data tests passed'
    return
end subroutine testStoreRetrievePairedData

subroutine assert(logical_test)
    logical :: logical_test
    if (.not.logical_test) then
        call abort
	end if
end subroutine assert
