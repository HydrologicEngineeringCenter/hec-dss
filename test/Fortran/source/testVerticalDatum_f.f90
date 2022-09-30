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

        subroutine extractVerticalDatumInfoFromUserHeader( &
            nativeDatum,                                   &
            unit,                                          &
            offsetToNavd88,                                &
            offsetToNavd88IsEstimate,                      &
            offsetToNgvd29,                                &
            offsetToNgvd29IsEstimate,                      &
            userHeader,                                    &
            userHeaderLen)
            character (len = 16), intent(in) :: nativeDatum
            character (len = 16), intent(in) :: unit
            real      (kind = 8), intent(in) :: offsetToNavd88
            integer   (kind = 4), intent(in) :: offsetToNavd88IsEstimate
            real      (kind = 8), intent(in) :: offsetToNgvd29
            integer   (kind = 4), intent(in) :: offsetToNgvd29IsEstimate
            integer   (kind = 4), intent(in) :: userHeader(*)
            integer   (kind = 4), intent(in) :: userHeaderLen
        end subroutine extractVerticalDatumInfoFromUserHeader

        subroutine getLocationVerticalDatumInfo( &
            nativeDatum,                                   &
            unit,                                          &
            offsetToNavd88,                                &
            offsetToNavd88IsEstimate,                      &
            offsetToNgvd29,                                &
            offsetToNgvd29IsEstimate,                      &
            fileTable,                                     &
            pathname)
            character (len = 16), intent(in)     :: nativeDatum
            character (len = 16), intent(in)     :: unit
            real      (kind = 8), intent(in)     :: offsetToNavd88
            integer   (kind = 4), intent(in)     :: offsetToNavd88IsEstimate
            real      (kind = 8), intent(in)     :: offsetToNgvd29
            integer   (kind = 4), intent(in)     :: offsetToNgvd29IsEstimate
            integer   (kind = 8), intent(in out) :: fileTable(*)
            character (len = *),  intent(in)     :: pathname
        end subroutine getLocationVerticalDatumInfo

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

        subroutine extractVdiFromUserHeader( &
            vdi,                             &
            userHeader,                      &
            userHeaderLen)
            type(verticalDatumInfo)        :: vdi
            integer (kind = 4), intent(in) :: userHeader(*)
            integer (kind = 4), intent(in) :: userHeaderLen
            integer (kind = 4)             :: offsetToNavd88IsEstimate
            integer (kind = 4)             :: offsetToNgvd29IsEstimate
            call extractVerticalDatumInfoFromUserHeader( &
                vdi%nativeDatum,                         &
                vdi%unit,                                &
                vdi%offsetToNavd88,                      &
                offsetToNavd88IsEstimate,                &
                vdi%offsetToNgvd29,                      &
                offsetToNgvd29IsEstimate,                &
                userHeader,                              &
                userHeaderLen)
            vdi%offsetToNavd88IsEstimate = offsetToNavd88IsEstimate /= 0
            vdi%offsetToNgvd29IsEstimate = offsetToNgvd29IsEstimate /= 0
        end subroutine extractVdiFromUserHeader

        subroutine getLocationVdi( &
            vdi,                   &
            fileTable,             &
            pathname)
            type(verticalDatumInfo)              :: vdi
            integer   (kind = 8), intent(in out) :: fileTable(*)
            character (len = *),  intent(in)     :: pathname
            integer   (kind = 4)                 :: offsetToNavd88IsEstimate
            integer   (kind = 4)                 :: offsetToNgvd29IsEstimate
            call getlocationverticaldatuminfo( &
                vdi%nativeDatum,               &
                vdi%unit,                      &
                vdi%offsetToNavd88,            &
                offsetToNavd88IsEstimate,      &
                vdi%offsetToNgvd29,            &
                offsetToNgvd29IsEstimate,      &
                fileTable,                     &
                pathname)
            vdi%offsetToNavd88IsEstimate = offsetToNavd88IsEstimate /= 0
            vdi%offsetToNgvd29IsEstimate = offsetToNgvd29IsEstimate /= 0
        end subroutine getLocationVdi

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
            integer i
            call bigEndian(i)
            isBigEndian = i.eq.1
        end function isBigEndian

        subroutine swapIfBigEndian(intArray, arraySize)
            integer (kind = 4), intent(in out) :: intArray(*)
            integer (kind = 4), intent(in)     :: arraySize
            integer (kind = 4)                 :: i
            if (isBigEndian()) then
                do i = 1, arraySize
                    call zswap6(intArray(i), intArray(i))
                end do
            end if
        end subroutine swapIfBigEndian

        character*(300) function vdiToString(vdi)
            implicit none
            type(verticalDatumInfo), intent(in) :: vdi
            character (len=300) errmsg
            if (vdi%nativeDatum == ' '.or.vdi%nativeDatum == CVD_UNSET) then
                vdiToString = ' '
            else
                call verticalDatumInfoToString(   &
                    vdiToString,                  &
                    errmsg,                       &
                    vdi%nativeDatum,              &
                    vdi%unit,                     &
                    vdi%offsetToNgvd29,           &
                    vdi%offsetToNgvd29IsEstimate, &
                    vdi%offsetToNavd88,           &
                    vdi%offsetToNavd88IsEstimate, &
                    .true.)    
            end if
        end function vdiToString
end module modVerticalDatumInfo

integer function test_vertical_datums_f()
    implicit none
    call testUserHeaderOps()
    call testStoreRetrieveTimeSeries()
    call testStoreRetrievePairedData()

    test_vertical_datums_f = 0
end function test_vertical_datums_f

subroutine testUserHeaderOps
    use modVerticalDatumInfo
    implicit none

    character (len=100) :: cheader, cvalue
    character (len=72)  :: cheaderShort
    integer   (kind=4)  :: iheader(25), iheaderShort(18), nhead, status, i

    equivalence (cheader, iheader)
    equivalence (cheaderShort, iheaderShort)

    cheader = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'
    call swapIfBigEndian(iheader, size(iheader))

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
    call swapIfBigEndian(iheader, size(iheader))
    call remove_user_header_param(iheader, nhead, size(iheader), 'firstParam')
    call swapIfBigEndian(iheader, size(iheader))
    call assert(cheader.eq.'secondParam:secondValue;thirdParam:thirdValue;')
    cheader = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'
    call swapIfBigEndian(iheader, size(iheader))
    call remove_user_header_param(iheader, nhead, size(iheader), 'secondParam')
    call swapIfBigEndian(iheader, size(iheader))
    call assert(cheader.eq.'firstParam:firstValue;thirdParam:thirdValue;')
    cheader = 'firstParam:firstValue;secondParam:secondValue;thirdParam:thirdValue;'
    call swapIfBigEndian(iheader, size(iheader))
    call remove_user_header_param(iheader, nhead, size(iheader), 'thirdParam')
    call swapIfBigEndian(iheader, size(iheader))
    call assert(cheader.eq.'firstParam:firstValue;secondParam:secondValue;')

    cheaderShort = 'firstParam:firstValue;secondParam:secondValuethirdParam:thirdValue;'
    call swapIfBigEndian(iheaderShort, size(iheaderShort))
    call set_user_header_param(iheaderShort, nhead, size(iheaderShort), 'fourthParam', '4thValue', status)
    call assert(status.ne.0)

    end subroutine testUserHeaderOps

character function zlocationPath(pathname)

    implicit none
    
    character (len=*), intent(in) :: pathname
    
    character (len=128) ca, cb, cc, cd, ce, cf
    integer (kind=4) na, nb, nc, nd, ne, nf, npath, status

    call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne, cf, nf, pathname, len_trim(pathname), status)
    call zfpn(ca, na, cb, nb, 'Location Info', 13, '', 0, '', 0, '', 0, zlocationPath, npath)
    
end function zlocationPath
    
subroutine deleteTimeSeriesRecords( &
    ifltab,                         &
    pathname,                       &
    startJul,                       &
    endJul,                         &
    deleteLocationRecordAlso)

    implicit none
    
    integer (kind=8),    intent(in out) :: ifltab(*)
    character (len=391), intent(in)     :: pathname
    integer (kind=4),    intent(in)     :: startJul, endJul
    logical (kind=4),    intent(in)     :: deleteLocationRecordAlso
    
    character (len=391) recordPathname
    character (len=128) ca, cb, cc, cd, ce, cf
    integer (kind=4) na, nb, nc, nd, ne, nf, npath, status
    integer (kind=4) yr, mo, da, blksiz, minblk, incblk, intvl, valueCount
    integer (kind=4) recordJul, intervalSeconds, iymdjl, zdssversion
    
    recordJul = 0
    recordPathname = ' '
    call zufpn(ca, na, cb, nb, cc, nc, cd, nd, ce, ne, cf, nf, pathname, len_trim(pathname), status)
    if (ce(1:1)=='~'.or.ce(1:1)=='I') then
        !-----------------------!
        ! irregular time series !
        !-----------------------!
        call zirbeg6 (ifltab, startJul, ce(:ne), yr, mo, da, blksiz, minblk, incblk)
    else
        !---------------------!
        ! regular time series !
        !---------------------!
        status = 1 ! interval minutes from E part
        call zgintl6(intvl, ce(:ne), valueCount, status)
        call zbegdt(startJul, intvl, yr, mo, da, blksiz, zdssVersion(ifltab) * 10000)
    end if
    recordJul = iymdjl(yr, mo, da)
    do while (recordJul <= endJul)
        nd = len(cd)
        call juldat(recordJul, 4, cd, nd)
        call zfpn(ca, na, cb, nb, cc, nc, cd, 9, ce, ne, cf, nf, recordPathname, npath)
        call zdelete(ifltab, recordPathname, status)
        call zincbk(1, recordJul, yr, mo, da)
    end do
    if (deleteLocationRecordAlso) then
    end if
end subroutine deleteTimeSeriesRecords

subroutine printTsTestInfo( &
    count,                  &
    expectSuccess,          &
    pathname,               &
    dataInFile,             &
    fileVdi,                &
    dataVdi,                &
    currentVerticalDatum,   &
    unit)

    use modVerticalDatumInfo
    implicit none
    
    integer (kind=4),         intent(in) :: count
    logical (kind=4),         intent(in) :: expectSuccess, dataInFile
    character (len=*),        intent(in) :: pathname
    type (verticalDatumInfo), intent(in) :: fileVdi, dataVdi
    character (len=*),        intent(in) :: currentVerticalDatum, unit
    
    if (expectSuccess) then
        write(*,'(a,i5,a)') 'Time series test ',count,' expecting SUCCESS'
    else
        write(*,'(a,i5,a)') 'Time series test ',count,' expecting ERROR'
    end if
    write(*, *) '    pathname               = '//pathname(:len_trim(pathname))
    write(*, *) '    data in file           =',dataInFile
    write(*, *) '    native datum in file   = '//fileVdi%nativeDatum
    write(*, *) '    incoming native datum  = '//dataVdi%nativeDatum
    write(*, *) '    incoming current datum = '//currentVerticalDatum
    write(*, *) '    incoming unit          = '//unit
end subroutine printTsTestInfo

subroutine printPdTestInfo( &
    count,                  &
    expectSuccess,          &
    pathname,               &
    dataInFile,             &
    fileVdi,                &
    dataVdi,                &
    currentVerticalDatum,   &
    indUnit,                &
    depUnit)

    use modVerticalDatumInfo
    implicit none
    
    integer (kind=4),         intent(in) :: count
    logical (kind=4),         intent(in) :: expectSuccess, dataInFile
    character (len=*),        intent(in) :: pathname
    type (verticalDatumInfo), intent(in) :: fileVdi, dataVdi
    character (len=*),        intent(in) :: currentVerticalDatum, indUnit, depUnit
    
    if (expectSuccess) then
        write(*,'(a,i5,a)') 'Paired data test ',count,' expecting SUCCESS'
    else
        write(*,'(a,i5,a)') 'Paired data test ',count,' expecting ERROR'
    end if
    write(*, *) '    pathname               = '//pathname(:len_trim(pathname))
    write(*, *) '    data in file           =',dataInFile
    write(*, *) '    native datum in file   = '//fileVdi%nativeDatum
    write(*, *) '    incoming native datum  = '//dataVdi%nativeDatum
    write(*, *) '    incoming current datum = '//currentVerticalDatum
    write(*, *) '    incoming units         = '//indUnit(:len_trim(indUnit))//', '//depUnit(:len_trim(depUnit))
end subroutine printPdTestInfo
    
subroutine testStoreRetrieveTimeSeries()
    use modVerticalDatumInfo
    implicit none

    integer (kind=8)        :: ifltab(250)
    integer (kind=4)        :: status, numberValues, i, j, k, k2, k3, kk, l, m, n, o, p, q, ii, len, iVerticalDatum
    integer (kind=4)        :: quality(24), itimes(6,2),userHeader(100), userHeaderLen, count, vdiCount
    integer (kind=4)        :: intervalOffset, compressionMethod, timesRetrieved(24), baseDate
    integer (kind=4)        :: startDay, endDay, recDay, iyr, imon, iday, ihm2m, na, nb, nc, nd, ne, nf, npath
    integer (kind=4)        :: intvl, iblock, minblk, incblk, numvals, iymdjl
    real (kind=8)           :: dvalues(6,3), dvals(24), dvals_out(24), offset
    real (kind=4)           :: fvalues(6,3), fvals(24), fvals_out(24)
    character (len=300)     :: errmsg, fileVdiStr, dataVdiStr
    character (len=80)      :: filename(2)
    character (len=391)     :: pathnames(2,2), recordPathname
    character (len=128)     :: ca, cb, cc, cd, ce, cf
    character (len=16)      :: unit(3), type, currentVerticalDatums(4), cVerticalDatum, nativeDatumInFile
    character (len=32)      :: unitSpec, unitSpec2
    character (len=16)      :: startDate, endDate, recDate
    character (len=4)       :: startTime, endTime
    character (len=400)     :: userHeaderStr
    logical                 :: readQuality, qualityWasRead, expectSuccess, dataInFile, lfound
    type(verticalDatumInfo) :: vdi(7), vdiInFile, blankVdi, thisVdi

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

    startDate = '01Oct2021'

    endDate = startDate

    startTime = '0100'

    endTime = '2400'

    currentVerticalDatums = (/CVD_NAVD88, CVD_NGVD29, 'Pensacola       ', CVD_UNSET/)
    
    vdiCount = size(vdi)

    call initVerticalDatumInfo(blankVdi)

    do j = 1, vdiCount
        call initVerticalDatumInfo(vdi(j))
        select case (j)
            case (1)
                vdi(j)%nativeDatum = CVD_NGVD29
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 0.3855
                vdi(j)%offsetToNavd88IsEstimate = .true.
                vdi(j)%offsetToNgvd29 = 0
                vdi(j)%offsetToNgvd29IsEstimate = .false.
            case (2)
                vdi(j)%nativeDatum = CVD_NGVD29
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNgvd29 = 0
                vdi(j)%offsetToNgvd29IsEstimate = .false.
            case (3)
                vdi(j)%nativeDatum = CVD_NAVD88
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 0
                vdi(j)%offsetToNavd88IsEstimate = .false.
                vdi(j)%offsetToNgvd29 = -0.3855
                vdi(j)%offsetToNgvd29IsEstimate = .true.
            case (4)
                vdi(j)%nativeDatum = CVD_NAVD88
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 0
                vdi(j)%offsetToNavd88IsEstimate = .false.
            case (5)
                vdi(j)%nativeDatum = 'Pensacola'
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 1.457
                vdi(j)%offsetToNavd88IsEstimate = .true.
                vdi(j)%offsetToNgvd29 = 1.07
                vdi(j)%offsetToNgvd29IsEstimate = .false.
            case (6)
                vdi(j)%nativeDatum = 'Pensacola'
                vdi(j)%unit = 'ft'
            case (7)
                ! just leave in initialized state
        end select
    end do
    !
    ! loop variables
    !
    ! i = DSS file version
    !     1 = DSS 6
    !     2 = DSS 7
    !
    ! j = vdi
    !     1 = NGVD-29 native with offset to NAVD-88
    !     2 = NGVD-29 native without offset to NAVD-88
    !     3 = NAVD-88 native with offset to NGVD-29
    !     4 = NAVD-88 native without offset to NGVD-29
    !     5 = OTHER native with local datum named "Pensacola" with offsets to NAVD-88 and NGVD-29
    !     6 = OTHER native with local datum named "Pensacola" without offsets to NAVD-88 and NGVD-29
    !     7 = None
    !
    ! k = vertical datum
    !     1 = NAVD-88
    !     2 = NGVD-29
    !     3 = OTHER (Pensacola)
    !     4 = UNSET
    !     k2  mod(k, 4) + 1
    !     k3  mod(k+1, 4) + 1
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
    ! q = ensure empty record
    !     1 = leave any existing data in file
    !     2 = delete existing record
    !
    call zset('MLVL', '', 1)
    count = 0
    do i = 1, 2
        call deletefile(filename(i), status)
        do j = 1, vdiCount
            do k = 1, 4
                k2 = mod(k, 4) + 1
                k3 = mod(k+1, 4) + 1
                do l = 1, 3
                    do m = 1, 3
                        do n = 1, 2
                            do o = 1, 2
                                do p = 1, 2
                                    do q = 1, 2
                                        count = count + 1
                                        ifltab = 0
                                        if (i == 1) then
                                            call zopen6(ifltab, filename(i), status)
                                        else
                                            call zopen7(ifltab, filename(i), status)
                                        end if
                                        call assert(status == 0)
                                        !------------------------------------------------------------------------------------!
                                        ! get whether data exists in file and the native datum in the file for this pathname !
                                        !------------------------------------------------------------------------------------!
                                        call zset('VDTM', CVD_UNSET, 0)
                                        numberValues = 6
                                        if (n == 1) then
                                            if (o == 1) then
                                                !-------------!
                                                ! RTS doubles !
                                                !-------------!
                                                dvals(1:numberValues) = dvalues(:,l)
                                                call zrrtsxd(          &
                                                    ifltab,            & ! IFLTAB  <-> file table
                                                    pathnames(o,n),    & ! CPATH    -> dataset name
                                                    startDate,         & ! CDATE    -> date of start of time window
                                                    startTime,         & ! CTIME    -> time of start of time window
                                                    numberValues,      & ! NVALS   <-> max number of values to retrieve / number of values retrieved
                                                    dvals_out,         & ! DVALS   <-  values array
                                                    quality,           & ! JQUAL   <-  quality flags array
                                                    readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                    qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                    unitSpec,          & ! CUNITS  <-  data unit
                                                    type,              & ! CTYPE   <-  data type
                                                    userHeader,        & ! IUHEAD  <-  user header array
                                                    size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                    userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                    intervalOffset,    & ! IOFSET  <-  offset into interval of the time of each value
                                                    compressionMethod, & ! JCOMP   <-  compression method used if values were compressed in file
                                                    status)              ! ISTAT   <-  status (0=success)
                                            else
                                                !------------!
                                                ! RTS floats !
                                                !------------!
                                                fvals(1:numberValues) = fvalues(:,l)
                                                call zrrtsx(           &
                                                    ifltab,            & ! IFLTAB  <-> file table
                                                    pathnames(o,n),    & ! CPATH    -> dataset name
                                                    startDate,         & ! CDATE    -> date of start of time window
                                                    startTime,         & ! CTIME    -> time of start of time window
                                                    numberValues,      & ! NVALS   <-> max number of values to retrieve / number of values retrieved
                                                    fvals_out,         & ! DVALS   <-  values array
                                                    quality,           & ! JQUAL   <-  quality flags array
                                                    readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                    qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                    unitSpec,          & ! CUNITS  <-  data unit
                                                    type,              & ! CTYPE   <-  data type
                                                    userHeader,        & ! IUHEAD  <-  user header array
                                                    size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                    userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                    intervalOffset,    & ! IOFSET  <-  offset into interval of the time of each value
                                                    compressionMethod, & ! JCOMP   <-  compression method used if values were compressed in file
                                                    status)              ! ISTAT   <-  status (0=success)
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
                                                    ifltab,            & ! IFLTAB  <-> file table
                                                    pathnames(o,n),    & ! CPATH    -> dataset name
                                                    startDay,          & ! JULS     -> days since 31Dec1899 of start of time window
                                                    ihm2m(startTime),  & ! ISTIME   -> minutes into day of start of time window
                                                    endDay,            & ! JULE     -> days since 31Dec1899 of end of time window
                                                    ihm2m(endTime),    & ! IETIME   -> minutes into day of end of time window
                                                    timesRetrieved,    & ! ITIMES  <-  times array as minutes offset from base date
                                                    dvals_out,         & ! DVALUES <-  values array
                                                    size(dvals_out),   & ! KVALS    -> max number of values to return
                                                    numberValues,      & ! NVALS   <-  number of values returned
                                                    baseDate,          & ! IBDATE  <-  days since 31Dec1899 of time of first value
                                                    quality,           & ! IQUAL   <-  quality flags
                                                    readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                    qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                    unitSpec,          & ! CUNITS  <-  data unit
                                                    type,              & ! CTYPE   <-  data type
                                                    userHeader,        & ! IUHEAD  <-  user header array
                                                    size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                    userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                    0,                 & ! INFLAG   -> read method (0=time window, 1=tw+prev, 2=tw+next 3=tw+prev+next)
                                                    status)              ! ISTAT   <-  status (0=success)
                                            else
                                                !------------!
                                                ! ITS floats !
                                                !------------!
                                                fvals(1:numberValues) = fvalues(:,l)
                                                call zritsx(           &
                                                    ifltab,            & ! IFLTAB  <-> file table
                                                    pathnames(o,n),    & ! CPATH    -> dataset name
                                                    startDay,          & ! JULS     -> days since 31Dec1899 of start of time window
                                                    ihm2m(startTime),  & ! ISTIME   -> minutes into day of start of time window
                                                    endDay,            & ! JULE     -> days since 31Dec1899 of end of time window
                                                    ihm2m(endTime),    & ! IETIME   -> minutes into day of end of time window
                                                    timesRetrieved,    & ! ITIMES  <-  times array as minutes offset from base date
                                                    fvals_out,         & ! SVALUES <-  values array
                                                    size(fvals_out),   & ! KVALS    -> max number of values to return
                                                    numberValues,      & ! NVALS   <-  number of values returned
                                                    baseDate,          & ! IBDATE  <-  days since 31Dec1899 of time of first value
                                                    quality,           & ! IQUAL   <-  quality flags
                                                    readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                    qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                    unitSpec,          & ! CUNITS  <-  data unit
                                                    type,              & ! CTYPE   <-  data type
                                                    userHeader,        & ! IUHEAD  <-  user header array
                                                    size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                    userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                    0,                 & ! INFLAG   -> read method (0=time window, 1=tw+prev, 2=tw+next 3=tw+prev+next)
                                                    status)              ! ISTAT   <-  status (0=success)
                                            end if
                                        end if
                                        dataInFile = status == 0
                                        if (i == 1) then
                                            !-------!
                                            ! DSS 6 !
                                            !-------!
                                            if (dataInFile) then
                                                call extractVdiFromUserHeader(vdiInFile, userHeader, userHeaderLen)
                                            else
                                                call initVerticalDatumInfo(vdiInFile)
                                            end if
                                        else
                                            !-------!
                                            ! DSS 7 !
                                            !-------!
                                            call getLocationVdi(vdiInFile, ifltab, pathnames(o,n))
                                        end if
                                        nativeDatumInFile = vdiInFile%nativeDatum
                                        userHeaderStr = ' '
                                        userHeaderLen = 0
                                        unitSpec = unit(l)
                                        kk = k
                                        !--------------------------------!
                                        ! set the default vertical datum !
                                        !--------------------------------!
                                        call zset('VDTM', currentVerticalDatums(kk), 0)
                                        if (p == 1) then
                                            !------------------------------------------------!
                                            ! add the vertical datum info to the user header !
                                            !------------------------------------------------!
                                            thisVdi = vdi(j)
                                            if (vdi(j)%nativeDatum /= ' ') then
                                                dataVdiStr = vdiToString(thisVdi)
                                                userHeaderStr = VERTICAL_DATUM_INFO_PARAM &
                                                    //':'//dataVdiStr(:len_trim(dataVdiStr))//';'
                                            end if
                                        else
                                            !------------------------------------------------------!
                                            ! don't add the vertical datum info to the user header !
                                            !------------------------------------------------------!
                                            thisVdi = blankVdi
                                        end if
                                        if (m > 1) then
                                            !----------------------------------------------------------!
                                            ! override the default vertical datum with the user header !
                                            !----------------------------------------------------------!
                                            kk = k2
                                            len = len_trim(userHeaderStr) + 1
                                            userHeaderStr(len:) = VERTICAL_DATUM_PARAM//':' // &
                                                currentVerticalDatums(kk)(:len_trim(currentVerticalDatums(kk)))//';'
                                            if (m > 2) then
                                                !--------------------------------------------------------!
                                                ! override default and user header datums with unit spec !
                                                !--------------------------------------------------------!
                                                kk = k3
                                                write(unitSpec,'(5a)')                     &
                                                    'U=',unit(l)(1:len_trim(unit(l))),'|', &
                                                    'V=',currentVerticalDatums(kk)
                                            end if
                                        end if
                                        if (q == 2) then
                                            !----------------------------!
                                            ! delete any records in file !
                                            !----------------------------!
                                            call datjul(startDate, startDay, status)
                                            call assert(status == 0)
                                            call datjul(endDate, endDay, status)
                                            call assert(status == 0)
                                            call deleteTimeSeriesRecords( &
                                                ifltab,                   &
                                                pathnames(o,n),           &
                                                startDay,                 &
                                                endDay,                   &
                                                .false.)
                                            dataInFile = .false.
                                            if (i == 1) then
                                                !--------------------------------------------------!
                                                ! deleting records also deletes file VDI for DSS 6 !
                                                !--------------------------------------------------!
                                                call initVerticalDatumInfo(vdiInFile)
                                                nativeDatumInFile = vdiInFile%nativeDatum
                                            end if
                                        end if
                                        !-----------------------------------------------!
                                        ! figure out whether the zs?tsx? should succeed !
                                        !-----------------------------------------------!
                                        call processStorageVdis(       &
                                            offset,                    &
                                            errmsg,                    &
                                            vdiToString(vdiInFile),    &
                                            vdiToString(thisVdi),      &
                                            currentVerticalDatums(kk), &
                                            dataInFile,                &
                                            unit(l))
                                        expectSuccess = errmsg == ' '
                                        !-------------------------------------------------------!
                                        ! store the time series in the specified vertical datum !
                                        !-------------------------------------------------------!
                                        call printTsTestInfo(count, expectSuccess, pathnames(o,n), dataInFile, vdiInFile, thisVdi, &
                                            currentVerticalDatums(kk), unitSpec)
                                        userHeaderLen = byteCountToIntCount(len_trim(userHeaderStr))
                                        call swapIfBigEndian(userHeader, userHeaderLen)
                                        numberValues = 6
                                        unitSpec2 = unitSpec
                                        if (n == 1) then
                                            if (o == 1) then
                                                !-------------!
                                                ! RTS doubles !
                                                !-------------!
                                                dvals(1:numberValues) = dvalues(:,l)
                                                call zsrtsxd(               &
                                                    ifltab,                 & ! IFLTAB   <-> file table
                                                    pathnames(o,n),         & ! CPATH     -> dataset name
                                                    startDate,              & ! CDATE     -> date of first value
                                                    startTime,              & ! CTIME     -> time of first value
                                                    numberValues,           & ! NVALS     -> number of values to store
                                                    dvals,                  & ! DVALUES   -> values to store
                                                    quality,                & ! JQUAL     -> quality flags to store
                                                    .true.,                 & ! LQUAL     -> whether to store quality flags (/1)
                                                    unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                    type,                   & ! CTYPE     -> data type
                                                    userHeader,             & ! IUHEAD    -> user header array
                                                    userHeaderLen,          & ! NUHEAD    -> number of header array elements to store
                                                    0,                      & ! IPLAN     -> data storage method 0=replace all)
                                                    0,                      & ! ICOMP     -> data compression type to use (0=file default)
                                                    0.,                     & ! BASEV     -> data compression base value for delta method
                                                    .false.,                & ! LBASEV    -> whether to use base value for delta method data compression (0/1)
                                                    .false.,                & ! LHIGH     -> whether to use 2 bytes per compressed value for delta method (0=let
                                                    0,                      & ! IPREC     -> base 10 exponent of compressed values for delta method
                                                    status)                   ! ISTAT    <-  status (0=success)
                                            else
                                                !------------!
                                                ! RTS floats !
                                                !------------!
                                                fvals(1:numberValues) = fvalues(:,l)
                                                call zsrtsx(                &
                                                    ifltab,                 & ! IFLTAB   <-> file table
                                                    pathnames(o,n),         & ! CPATH     -> dataset name
                                                    startDate,              & ! CDATE     -> date of first value
                                                    startTime,              & ! CTIME     -> time of first value
                                                    numberValues,           & ! NVALS     -> number of values to store
                                                    fvals,                  & ! VALUES    -> values to store
                                                    quality,                & ! JQUAL     -> quality flags to store
                                                    .true.,                 & ! LQUAL     -> whether to store quality flags (/1)
                                                    unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                    type,                   & ! CTYPE     -> data type
                                                    userHeader,             & ! IUHEAD    -> user header array
                                                    userHeaderLen,          & ! NUHEAD    -> number of header array elements to store
                                                    0,                      & ! IPLAN     -> data storage method 0=replace all)
                                                    0,                      & ! ICOMP     -> data compression type to use (0=file default)
                                                    0.,                     & ! BASEV     -> data compression base value for delta method
                                                    .false.,                & ! LBASEV    -> whether to use base value for delta method data compression (0/1)
                                                    .false.,                & ! LHIGH     -> whether to use 2 bytes per compressed value for delta method (0=let
                                                    0,                      & ! IPREC     -> base 10 exponent of compressed values for delta method
                                                    status)                   ! ISTAT    <-  status (0=success)
                                            end if
                                        else
                                            if (o == 1) then
                                                !-------------!
                                                ! ITS doubles !
                                                !-------------!
                                                dvals(1:numberValues) = dvalues(:,l)
                                                call zsitsxd(               &
                                                    ifltab,                 & ! IFLTAB  <-> file table
                                                    pathnames(o,n),         & ! CPATH    -> dataset name
                                                    itimes,                 & ! ITIMES   -> times relative to base date
                                                    dvals,                  & ! DVALUES  -> values to store
                                                    numberValues,           & ! NVALUE   -> number of values to store
                                                    0,                      & ! IBDATE   -> base date for times
                                                    quality,                & ! JQUAL    -> quality flags to store
                                                    .true.,                 & ! LSQUAL   -> whether to store quality flags (0/1)
                                                    unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                    type,                   & ! CTYPE     -> data type
                                                    userHeader,             & ! IHEADU    -> user header array
                                                    userHeaderLen,          & ! NHEADU    -> number of header array elements to store
                                                    1,                      & ! INFLAG   -> data storage method (0=merge)
                                                    status)                   ! ISTAT   <-  status (0=success)
                                            else
                                                !------------!
                                                ! ITS floats !
                                                !------------!
                                                fvals(1:numberValues) = fvalues(:,l)
                                                call zsitsx(                &
                                                    ifltab,                 & ! IFLTAB  <-> file table
                                                    pathnames(o,n),         & ! CPATH    -> dataset name
                                                    itimes,                 & ! ITIMES   -> times relative to base date
                                                    fvals,                  & ! VALUES   -> values to store
                                                    numberValues,           & ! NVALUE   -> number of values to store
                                                    0,                      & ! IBDATE   -> base date for times
                                                    quality,                & ! JQUAL    -> quality flags to store
                                                    .true.,                 & ! LSQUAL   -> whether to store quality flags (0/1)
                                                    unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                    type,                   & ! CTYPE     -> data type
                                                    userHeader,             & ! IHEADU    -> user header array
                                                    userHeaderLen,          & ! NHEADU    -> number of header array elements to store
                                                    1,                      & ! INFLAG   -> data storage method (0=merge)
                                                    status)                   ! ISTAT   <-  status (0=success)
                                            end if
                                        end if
                                        call assert((status == 0) .eqv. expectSuccess)
                                        if (status /= 0) then
                                            if (index(errmsg, 'Data native datum') > 0 .and. &
                                                index(errmsg, 'conflicts with file native datum') > 0) then
                                                !--------------------------------------!
                                                ! change of vertical datum information !
                                                !                                      !
                                                ! set VDOW to override file VDI        !
                                                ! with data VDI and re-try             !
                                                !--------------------------------------!
                                                call zset('VDOW', ' ', 1)
                                                count = count + 1
                                                call processStorageVdis(       &
                                                    offset,                    &
                                                    errmsg,                    &
                                                    vdiToString(vdiInFile),    &
                                                    vdiToString(thisVdi),      &
                                                    currentVerticalDatums(kk), &
                                                    dataInFile,                &
                                                    unit(l))
                                                expectSuccess = errmsg == ' '
                                                unitSpec2 = unitSpec
                                                if (n == 1) then
                                                    if (o == 1) then
                                                        !-------------!
                                                        ! RTS doubles !
                                                        !-------------!
                                                        dvals(1:numberValues) = dvalues(:,l)
                                                        call zsrtsxd(               &
                                                            ifltab,                 & ! IFLTAB   <-> file table
                                                            pathnames(o,n),         & ! CPATH     -> dataset name
                                                            startDate,              & ! CDATE     -> date of first value
                                                            startTime,              & ! CTIME     -> time of first value
                                                            numberValues,           & ! NVALS     -> number of values to store
                                                            dvals,                  & ! DVALUES   -> values to store
                                                            quality,                & ! JQUAL     -> quality flags to store
                                                            .true.,                 & ! LQUAL     -> whether to store quality flags (/1)
                                                            unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                            type,                   & ! CTYPE     -> data type
                                                            userHeader,             & ! IUHEAD    -> user header array
                                                            userHeaderLen,          & ! NUHEAD    -> number of header array elements to store
                                                            0,                      & ! IPLAN     -> data storage method 0=replace all)
                                                            0,                      & ! ICOMP     -> data compression type to use (0=file default)
                                                            0.,                     & ! BASEV     -> data compression base value for delta method
                                                            .false.,                & ! LBASEV    -> whether to use base value for delta method data compression (0/1)
                                                            .false.,                & ! LHIGH     -> whether to use 2 bytes per compressed value for delta method (0=let
                                                            0,                      & ! IPREC     -> base 10 exponent of compressed values for delta method
                                                            status)                   ! ISTAT    <-  status (0=success)
                                                    else
                                                        !------------!
                                                        ! RTS floats !
                                                        !------------!
                                                        fvals(1:numberValues) = fvalues(:,l)
                                                        call zsrtsx(                &
                                                            ifltab,                 & ! IFLTAB   <-> file table
                                                            pathnames(o,n),         & ! CPATH     -> dataset name
                                                            startDate,              & ! CDATE     -> date of first value
                                                            startTime,              & ! CTIME     -> time of first value
                                                            numberValues,           & ! NVALS     -> number of values to store
                                                            fvals,                  & ! VALUES    -> values to store
                                                            quality,                & ! JQUAL     -> quality flags to store
                                                            .true.,                 & ! LQUAL     -> whether to store quality flags (/1)
                                                            unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                            type,                   & ! CTYPE     -> data type
                                                            userHeader,             & ! IUHEAD    -> user header array
                                                            userHeaderLen,          & ! NUHEAD    -> number of header array elements to store
                                                            0,                      & ! IPLAN     -> data storage method 0=replace all)
                                                            0,                      & ! ICOMP     -> data compression type to use (0=file default)
                                                            0.,                     & ! BASEV     -> data compression base value for delta method
                                                            .false.,                & ! LBASEV    -> whether to use base value for delta method data compression (0/1)
                                                            .false.,                & ! LHIGH     -> whether to use 2 bytes per compressed value for delta method (0=let
                                                            0,                      & ! IPREC     -> base 10 exponent of compressed values for delta method
                                                            status)                   ! ISTAT    <-  status (0=success)
                                                    end if
                                                else
                                                    if (o == 1) then
                                                        !-------------!
                                                        ! ITS doubles !
                                                        !-------------!
                                                        dvals(1:numberValues) = dvalues(:,l)
                                                        call zsitsxd(               &
                                                            ifltab,                 & ! IFLTAB  <-> file table
                                                            pathnames(o,n),         & ! CPATH    -> dataset name
                                                            itimes,                 & ! ITIMES   -> times relative to base date
                                                            dvals,                  & ! DVALUES  -> values to store
                                                            numberValues,           & ! NVALUE   -> number of values to store
                                                            0,                      & ! IBDATE   -> base date for times
                                                            quality,                & ! JQUAL    -> quality flags to store
                                                            .true.,                 & ! LSQUAL   -> whether to store quality flags (0/1)
                                                            unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                            type,                   & ! CTYPE     -> data type
                                                            userHeader,             & ! IHEADU    -> user header array
                                                            userHeaderLen,          & ! NHEADU    -> number of header array elements to store
                                                            1,                      & ! INFLAG   -> data storage method (0=merge)
                                                            status)                   ! ISTAT   <-  status (0=success)
                                                    else
                                                        !------------!
                                                        ! ITS floats !
                                                        !------------!
                                                        fvals(1:numberValues) = fvalues(:,l)
                                                        call zsitsx(                &
                                                            ifltab,                 & ! IFLTAB  <-> file table
                                                            pathnames(o,n),         & ! CPATH    -> dataset name
                                                            itimes,                 & ! ITIMES   -> times relative to base date
                                                            fvals,                  & ! VALUES   -> values to store
                                                            numberValues,           & ! NVALUE   -> number of values to store
                                                            0,                      & ! IBDATE   -> base date for times
                                                            quality,                & ! JQUAL    -> quality flags to store
                                                            .true.,                 & ! LSQUAL   -> whether to store quality flags (0/1)
                                                            unitSpec2,              & ! CUNITS   <-> data unit (may be modified to remove current datum)
                                                            type,                   & ! CTYPE     -> data type
                                                            userHeader,             & ! IHEADU    -> user header array
                                                            userHeaderLen,          & ! NHEADU    -> number of header array elements to store
                                                            1,                      & ! INFLAG   -> data storage method (0=merge)
                                                            status)                   ! ISTAT   <-  status (0=success)
                                                    end if
                                                end if
                                                call zset('VDOW', ' ', 0)
                                                call assert((status == 0) .eqv. expectSuccess)
                                            end if
                                        end if
                                        call zclose(ifltab)
                                        if (status == 0) then
                                            !------------------------------------------------------------!
                                            ! set the default vertical datum to the datum we stored with !
                                            !------------------------------------------------------------!
                                            call zset('VDTM', currentVerticalDatums(kk), 0)
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
                                                        ifltab,            & ! IFLTAB  <-> file table
                                                        pathnames(o,n),    & ! CPATH    -> dataset name
                                                        startDate,         & ! CDATE    -> date of start of time window
                                                        startTime,         & ! CTIME    -> time of start of time window
                                                        numberValues,      & ! NVALS   <-> max number of values to retrieve / number of values retrieved
                                                        dvals_out,         & ! DVALS   <-  values array
                                                        quality,           & ! JQUAL   <-  quality flags array
                                                        readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                        qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                        unitSpec2,         & ! CUNITS  <-  data unit
                                                        type,              & ! CTYPE   <-  data type
                                                        userHeader,        & ! IUHEAD  <-  user header array
                                                        size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                        userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                        intervalOffset,    & ! IOFSET  <-  offset into interval of the time of each value
                                                        compressionMethod, & ! JCOMP   <-  compression method used if values were compressed in file
                                                        status)              ! ISTAT   <-  status (0=success)
                                                else
                                                    !------------!
                                                    ! RTS floats !
                                                    !------------!
                                                    fvals(1:numberValues) = fvalues(:,l)
                                                    call zrrtsx(           &
                                                        ifltab,            & ! IFLTAB  <-> file table
                                                        pathnames(o,n),    & ! CPATH    -> dataset name
                                                        startDate,         & ! CDATE    -> date of start of time window
                                                        startTime,         & ! CTIME    -> time of start of time window
                                                        numberValues,      & ! NVALS   <-> max number of values to retrieve / number of values retrieved
                                                        fvals_out,         & ! DVALS   <-  values array
                                                        quality,           & ! JQUAL   <-  quality flags array
                                                        readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                        qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                        unitSpec2,         & ! CUNITS  <-  data unit
                                                        type,              & ! CTYPE   <-  data type
                                                        userHeader,        & ! IUHEAD  <-  user header array
                                                        size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                        userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                        intervalOffset,    & ! IOFSET  <-  offset into interval of the time of each value
                                                        compressionMethod, & ! JCOMP   <-  compression method used if values were compressed in file
                                                        status)              ! ISTAT   <-  status (0=success)
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
                                                        ifltab,            & ! IFLTAB  <-> file table
                                                        pathnames(o,n),    & ! CPATH    -> dataset name
                                                        startDay,          & ! JULS     -> days since 31Dec1899 of start of time window
                                                        ihm2m(startTime),  & ! ISTIME   -> minutes into day of start of time window
                                                        endDay,            & ! JULE     -> days since 31Dec1899 of end of time window
                                                        ihm2m(endTime),    & ! IETIME   -> minutes into day of end of time window
                                                        timesRetrieved,    & ! ITIMES  <-  times array as minutes offset from base date
                                                        dvals_out,         & ! DVALUES <-  values array
                                                        size(dvals_out),   & ! KVALS    -> max number of values to return
                                                        numberValues,      & ! NVALS   <-  number of values returned
                                                        baseDate,          & ! IBDATE  <-  days since 31Dec1899 of time of first value
                                                        quality,           & ! IQUAL   <-  quality flags
                                                        readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                        qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                        unitSpec2,         & ! CUNITS  <-  data unit
                                                        type,              & ! CTYPE   <-  data type
                                                        userHeader,        & ! IUHEAD  <-  user header array
                                                        size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                        userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                        0,                 & ! INFLAG   -> read method (0=time window, 1=tw+prev, 2=tw+next 3=tw+prev+next)
                                                        status)              ! ISTAT   <-  status (0=success)
                                                else
                                                    !------------!
                                                    ! ITS floats !
                                                    !------------!
                                                    fvals(1:numberValues) = fvalues(:,l)
                                                    call zritsx(           &
                                                        ifltab,            & ! IFLTAB  <-> file table
                                                        pathnames(o,n),    & ! CPATH    -> dataset name
                                                        startDay,          & ! JULS     -> days since 31Dec1899 of start of time window
                                                        ihm2m(startTime),  & ! ISTIME   -> minutes into day of start of time window
                                                        endDay,            & ! JULE     -> days since 31Dec1899 of end of time window
                                                        ihm2m(endTime),    & ! IETIME   -> minutes into day of end of time window
                                                        timesRetrieved,    & ! ITIMES  <-  times array as minutes offset from base date
                                                        fvals_out,         & ! SVALUES <-  values array
                                                        size(fvals_out),   & ! KVALS    -> max number of values to return
                                                        numberValues,      & ! NVALS   <-  number of values returned
                                                        baseDate,          & ! IBDATE  <-  days since 31Dec1899 of time of first value
                                                        quality,           & ! IQUAL   <-  quality flags
                                                        readQuality,       & ! LQUAL    -> whether to retrieve quality flags if they exist (0/1)
                                                        qualityWasRead,    & ! LQREAD  <-  whether quality flags were retrieved (0/1)
                                                        unitSpec2,         & ! CUNITS  <-  data unit
                                                        type,              & ! CTYPE   <-  data type
                                                        userHeader,        & ! IUHEAD  <-  user header array
                                                        size(userHeader),  & ! KUHEAD   -> max number of user header elements to retrieve
                                                        userHeaderLen,     & ! NUHEAD  <-  number of user header elements retrieved
                                                        0,                 & ! INFLAG   -> read method (0=time window, 1=tw+prev, 2=tw+next 3=tw+prev+next)
                                                        status)              ! ISTAT   <-  status (0=success)
                                                end if
                                            end if
                                            call zclose(ifltab)
                                            call assert(status == 0)
                                            !------------------------------------------------------!
                                            ! compare the retrieved time series to what was stored !
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
    end do
    write(0,'(/,/,i5,a,/,/)') count,' time sereies tests passed'
    return
end subroutine testStoreRetrieveTimeSeries

subroutine testStoreRetrievePairedData()
    use modVerticalDatumInfo
    implicit none

    integer (kind=8)        :: ifltab(250)
    integer (kind=4)        :: status, numberOrdinates, numberCurves, ihoriz, nords, ncurves, nvals, count, iVerticalDatum
    integer (kind=4)        :: userHeader(100), userHeaderLen, i, j, k, k2, k3, kk, l, m, n, o, p, q, ii, len, vdiCount
    real (kind=8)           :: dordinates(6,3), dvalues(6,3), dvals(12), dvals_out(12), offset
    real (kind=4)           :: fordinates(6,3), fvalues(6,3), fvals(12), fvals_out(12)
    character (len=300)     :: errmsg, fileVdiStr, dataVdiStr
    character (len=80)      :: filename(2)
    character (len=80)      :: pathnames(2,2)
    character (len=16)      :: unit(3), type, currentVerticalDatums(4), cVerticalDatum, nativeDatumInFile
    character (len=32)      :: unitSpec, c1unit, c2unit, c1type, c2type, clabel
    character (len=4)       :: startTime, endTime
    character (len=400)     :: userHeaderStr
    logical                 :: l_label, expectSuccess, dataInFile, lfound
    type(verticalDatumInfo) :: vdi(7), vdiInFile, blankVdi, thisVdi


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

    currentVerticalDatums = (/CVD_NAVD88, CVD_NGVD29, 'Pensacola       ', CVD_UNSET/)

    vdiCount = size(vdi)

    call initVerticalDatumInfo(blankVdi)

    do j = 1, vdiCount
        call initVerticalDatumInfo(vdi(j))
        select case (j)
            case (1)
                vdi(j)%nativeDatum = CVD_NGVD29
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 0.3855
                vdi(j)%offsetToNavd88IsEstimate = .true.
                vdi(j)%offsetToNgvd29 = 0
                vdi(j)%offsetToNgvd29IsEstimate = .false.
            case (2)
                vdi(j)%nativeDatum = CVD_NGVD29
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNgvd29 = 0
                vdi(j)%offsetToNgvd29IsEstimate = .false.
            case (3)
                vdi(j)%nativeDatum = CVD_NAVD88
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 0
                vdi(j)%offsetToNavd88IsEstimate = .false.
                vdi(j)%offsetToNgvd29 = -0.3855
                vdi(j)%offsetToNgvd29IsEstimate = .true.
            case (4)
                vdi(j)%nativeDatum = CVD_NAVD88
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 0
                vdi(j)%offsetToNavd88IsEstimate = .false.
            case (5)
                vdi(j)%nativeDatum = 'Pensacola'
                vdi(j)%unit = 'ft'
                vdi(j)%offsetToNavd88 = 1.457
                vdi(j)%offsetToNavd88IsEstimate = .true.
                vdi(j)%offsetToNgvd29 = 1.07
                vdi(j)%offsetToNgvd29IsEstimate = .false.
            case (6)
                vdi(j)%nativeDatum = 'Pensacola'
                vdi(j)%unit = 'ft'
            case (7)
                ! just leave in initialized state
        end select
    end do
    !
    ! loop variables
    !
    ! i = DSS file version
    !     1 = DSS 6
    !     2 = DSS 7
    !
    ! j = vdi
    !     1 = NGVD-29 native with offset to NAVD-88
    !     2 = NGVD-29 native without offset to NAVD-88
    !     3 = NAVD-88 native with offset to NGVD-29
    !     4 = NAVD-88 native without offset to NGVD-29
    !     5 = OTHER native with local datum named "Pensacola" with offsets to NAVD-88 and NGVD-29
    !     6 = OTHER native with local datum named "Pensacola" without offsets to NAVD-88 and NGVD-29
    !     7 = None
    !
    ! k = vertical datum
    !     1 = NAVD-88
    !     2 = NGVD-29
    !     3 = OTHER (Pensacola)
    !     4 = UNSET
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
    ! q = ensure empty record
    !     1 = leave any existing data in file
    !     2 = delete existing record
    !
    call zset('MLVL', '', 1)
    count = 0
    do i = 1, 2
        call deletefile(filename(i), status)
        do j = 1, vdiCount
            do k = 1, 4
                k2 = mod(k, 4) + 1
                k3 = mod(k+1, 4) + 1
                do l = 1, 3
                    unitSpec = unit(l)
                    do m = 1, 3
                        do n = 1, 2
                            do o = 1, 2
                                do p = 1, 2
                                    do q = 1, 2
                                        count = count + 1
                                        ifltab = 0
                                        if (i == 1) then
                                            call zopen6(ifltab, filename(i), status)
                                        else
                                            call zopen7(ifltab, filename(i), status)
                                        end if
                                        call assert(status == 0)
                                        !------------------------------------------------------------------------------------!
                                        ! get whether data exists in file and the native datum in the file for this pathname !
                                        !------------------------------------------------------------------------------------!
                                        call zset('VDTM', CVD_UNSET, 0)
                                        nords = 0
                                        ncurves = 0
                                        nvals = 0
                                        if (o == 1) then
                                            !---------!
                                            ! doubles !
                                            !---------!
                                            call zrpdd(           &
                                                ifltab,           & ! IFLTAB  <-> file table
                                                pathnames(o,n),   & ! CPATH    -> record name
                                                nords,            & ! NORD    <-  number of ordinates
                                                ncurves,          & ! NCURVE  <-  number of curves
                                                ihoriz,           & ! IHORIZ  <-  which var plots on horizontals axis (1=ordinates, 2=values)
                                                c1unit,           & ! C1UNIT  <-  unit of ordinates
                                                c1type,           & ! C1TYPE  <-  data type of ordinates
                                                c2unit,           & ! C2UNIT  <-  unit of curve values
                                                c2type,           & ! C2TYPE  <-  data type of curve values
                                                dvals_out,        & ! DVALUES <-  1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                size(dvals),      & ! KVALS    -> size of dvals_out must be at least (ncurve+1) * nords
                                                nvals,            & ! NVALS   <-  number of ordinates + curve values retrieved
                                                clabel,           & ! CLABEL  <-  array of labels for each curve
                                                0,                & ! KLABEL   -> max number of labels to retrieve
                                                l_label,          & ! LABEL   <-  whether labels were retrieved
                                                userHeader,       & ! IUHEAD  <-  user header array
                                                size(userHeader), & ! KUHEAD   -> max number of user header elements to retrieve
                                                userHeaderLen,    & ! NUHEAD  <-  number of user header elements retrieved
                                                status)             ! ISTAT   <-  status (0=success)
                                        else
                                            !--------!
                                            ! floats !
                                            !--------!
                                            call zrpd(            &
                                                ifltab,           & ! IFLTAB  <-> file table
                                                pathnames(o,n),   & ! CPATH    -> record name
                                                nords,            & ! NORD    <-  number of ordinates
                                                ncurves,          & ! NCURVE  <-  number of curves
                                                ihoriz,           & ! IHORIZ  <-  which var plots on horizontals axis (1=ordinates, 2=values)
                                                c1unit,           & ! C1UNIT  <-  unit of ordinates
                                                c1type,           & ! C1TYPE  <-  data type of ordinates
                                                c2unit,           & ! C2UNIT  <-  unit of curve values
                                                c2type,           & ! C2TYPE  <-  data type of curve values
                                                fvals_out,        & ! SVALUES <-  1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                size(fvals),      & ! KVALS    -> size of dvals_out must be at least (ncurve+1) * nords
                                                nvals,            & ! NVALS   <-  number of ordinates + curve values retrieved
                                                clabel,           & ! CLABEL  <-  array of labels for each curve
                                                0,                & ! KLABEL   -> max number of labels to retrieve
                                                l_label,          & ! LABEL   <-  whether labels were retrieved
                                                userHeader,       & ! IUHEAD  <-  user header array
                                                size(userHeader), & ! KUHEAD   -> max number of user header elements to retrieve
                                                userHeaderLen,    & ! NUHEAD  <-  number of user header elements retrieved
                                                status)             ! ISTAT   <-  status (0=success)
                                        end if
                                        dataInFile = status == 0
                                        if (i == 1) then
                                            !-------!
                                            ! DSS 6 !
                                            !-------!
                                            if (dataInFile) then
                                                call extractVdiFromUserHeader(vdiInFile, userHeader, userHeaderLen)
                                            else
                                                call initVerticalDatumInfo(vdiInFile)
                                            end if
                                        else
                                            !-------!
                                            ! DSS 7 !
                                            !-------!
                                            call getLocationVdi(vdiInFile, ifltab, pathnames(o,n))
                                        end if
                                        nativeDatumInFile = vdiInFile%nativeDatum
                                        userHeaderStr = ' '
                                        userHeaderLen = 0
                                        kk = k
                                        !--------------------------------!
                                        ! set the default vertical datum !
                                        !--------------------------------!
                                        call zset('VDTM', currentVerticalDatums(kk), 0)
                                        if (p == 1) then
                                            !------------------------------------------------!
                                            ! add the vertical datum info to the user header !
                                            !------------------------------------------------!
                                            thisVdi = vdi(j)  
                                            if (vdi(j)%nativeDatum /= ' ') then
                                                dataVdiStr = vdiToString(thisVdi)
                                                userHeaderStr = VERTICAL_DATUM_INFO_PARAM &
                                                    //':'//dataVdiStr(:len_trim(dataVdiStr))//';'
                                            end if
                                        else
                                            !------------------------------------------------------!
                                            ! don't add the vertical datum info to the user header !
                                            !------------------------------------------------------!
                                            thisVdi = blankVdi  
                                        end if
                                        c1unit = unit(l)
                                        c2unit = unit(l)
                                        if (m > 1) then
                                            !----------------------------------------------------------!
                                            ! override the default vertical datum with the user header !
                                            !----------------------------------------------------------!
                                            kk = k2
                                            len = len_trim(userHeaderStr) + 1
                                            userHeaderStr(len:) = VERTICAL_DATUM_PARAM//':'// &
                                                currentVerticalDatums(kk)(:len_trim(currentVerticalDatums(kk)))//';'
                                            if (m > 2) then
                                                !--------------------------------------------------------!
                                                ! override default and user header datums with unit spec !
                                                !--------------------------------------------------------!
                                                kk = k3
                                                write(unitSpec,'(5a)')                     &
                                                    'U=',unit(l)(1:len_trim(unit(l))),'|', &
                                                    'V=',currentVerticalDatums(kk)
                                                if (n == 1) then
                                                    c2unit = unitSpec
                                                else
                                                    c1unit = unitSpec
                                                end if
                                            end if
                                        end if
                                        if (q == 2) then
                                            !----------------------------!
                                            ! delete any records in file !
                                            !----------------------------!
                                            if (dataInFile) then
                                                call zdelete(ifltab, pathnames(o,n), status)
                                                call assert(status == 0)
                                            end if
                                            dataInFile = .false.
                                            if (i == 1) then
                                                !--------------------------------------------------!
                                                ! deleting records also deletes file VDI for DSS 6 !
                                                !--------------------------------------------------!
                                                call initVerticalDatumInfo(vdiInFile)
                                                nativeDatumInFile = vdiInFile%nativeDatum
                                            end if
                                        end if
                                        !-----------------------------------------------!
                                        ! figure out whether the zs?tsx? should succeed !
                                        !-----------------------------------------------!
                                        call processStorageVdis(       &
                                            offset,                    &
                                            errmsg,                    &
                                            vdiToString(vdiInFile),    &
                                            vdiToString(thisVdi),      &
                                            currentVerticalDatums(kk), &
                                            dataInFile,                &
                                            unit(l))
                                        expectSuccess = errmsg == ' '
                                        !-------------------------------------------------------!
                                        ! store the paried data in the specified vertical datum !
                                        !-------------------------------------------------------!
                                        call printPdTestInfo(count, expectSuccess, pathnames(o,n), dataInFile, vdiInFile, thisVdi, &
                                            currentVerticalDatums(kk), c1unit, c2unit)
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
                                            call zspdd(          & 
                                                ifltab,          & ! IFLTAB  <-> file table
                                                pathnames(o,n),  & ! CPATH    -> record name
                                                numberOrdinates, & ! NORD     -> number of ordinates
                                                numberCurves,    & ! NCURVE   -> number of curves
                                                1,               & ! IHORIZ   -> which var plots on horizontals axis (1=ordinates, 2=values)
                                                c1unit,          & ! C1UNIT  <-> unit of ordinates  (may be modified to remove current datum)
                                                type,            & ! C1TYPE   -> data type of ordinates
                                                c2unit,          & ! C2UNIT  <-> unit of curve values  (may be modified to remove current datum)
                                                type,            & ! C2TYPE   -> data type of curve values
                                                dvals,           & ! DVALUES  -> 1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                '',              & ! CLABEL   -> array of labels for each curve
                                                .false.,         & ! LABEL    -> whether to store labels
                                                userHeader,      & ! IUHEAD   -> user header array
                                                userHeaderLen,   & ! NUHEAD   -> number of user header elements to store
                                                0,               & ! IPLAN    -> storage plan (0=always store, 1=only create new, 2=only overwrite existing)
                                                status)            ! ISTAT   <-  status (0=success) 
                                        else
                                            !--------!
                                            ! floats !
                                            !--------!
                                            fvals(1:6) = fordinates(:,l)
                                            fvals(7:12) = fvalues(:,l)
                                            call zspd(           & 
                                                ifltab,          & ! IFLTAB  <-> file table
                                                pathnames(o,n),  & ! CPATH    -> record name
                                                numberOrdinates, & ! NORD     -> number of ordinates
                                                numberCurves,    & ! NCURVE   -> number of curves
                                                1,               & ! IHORIZ   -> which var plots on horizontals axis (1=ordinates, 2=values)
                                                c1unit,          & ! C1UNIT  <-> unit of ordinates  (may be modified to remove current datum)
                                                type,            & ! C1TYPE   -> data type of ordinates
                                                c2unit,          & ! C2UNIT  <-> unit of curve values  (may be modified to remove current datum)
                                                type,            & ! C2TYPE   -> data type of curve values
                                                fvals,           & ! SVALUES  -> 1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                '',              & ! CLABEL   -> array of labels for each curve
                                                .false.,         & ! LABEL    -> whether to store labels
                                                userHeader,      & ! IUHEAD   -> user header array
                                                userHeaderLen,   & ! NUHEAD   -> number of user header elements to store
                                                0,               & ! IPLAN    -> storage plan (0=always store, 1=only create new, 2=only overwrite existing)
                                                status)            ! ISTAT   <-  status (0=success) 
                                        end if
                                        call assert((status == 0) .eqv. expectSuccess)
                                        if (status /= 0) then
                                            if (index(errmsg, 'Data native datum') > 0 .and. &
                                                index(errmsg, 'conflicts with file native datum') > 0) then
                                                !--------------------------------------!
                                                ! change of vertical datum information !
                                                !                                      !
                                                ! set VDOW to override file VDI        !
                                                ! with data VDI and re-try             !
                                                !--------------------------------------!
                                                call zset('VDOW', ' ', 1)
                                                count = count + 1
                                                call processStorageVdis(       &
                                                    offset,                    &
                                                    errmsg,                    &
                                                    vdiToString(vdiInFile),    &
                                                    vdiToString(thisVdi),      &
                                                    currentVerticalDatums(kk), &
                                                    dataInFile,                &
                                                    unit(l))
                                                expectSuccess = errmsg == ' '
                                                if (m > 2) then
                                                    !-----------------------------------------------!
                                                    ! unit spec has already been removed from units !
                                                    !-----------------------------------------------!
                                                    if (n == 1) then
                                                        c2unit = unitSpec
                                                    else
                                                        c1unit = unitSpec
                                                    end if
                                                end if
                                                call processStorageVdis(       &
                                                    offset,                    &
                                                    errmsg,                    &
                                                    vdiToString(vdiInFile),    &
                                                    vdiToString(thisVdi),      &
                                                    currentVerticalDatums(kk), &
                                                    dataInFile,                &
                                                    unit(l))
                                                expectSuccess = errmsg == ' '
                                                if (o == 1) then
                                                    !---------!
                                                    ! doubles !
                                                    !---------!
                                                    call zspdd(          & 
                                                        ifltab,          & ! IFLTAB  <-> file table
                                                        pathnames(o,n),  & ! CPATH    -> record name
                                                        numberOrdinates, & ! NORD     -> number of ordinates
                                                        numberCurves,    & ! NCURVE   -> number of curves
                                                        1,               & ! IHORIZ   -> which var plots on horizontals axis (1=ordinates, 2=values)
                                                        c1unit,          & ! C1UNIT  <-> unit of ordinates  (may be modified to remove current datum)
                                                        type,            & ! C1TYPE   -> data type of ordinates
                                                        c2unit,          & ! C2UNIT  <-> unit of curve values  (may be modified to remove current datum)
                                                        type,            & ! C2TYPE   -> data type of curve values
                                                        dvals,           & ! DVALUES  -> 1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                        '',              & ! CLABEL   -> array of labels for each curve
                                                        .false.,         & ! LABEL    -> whether to store labels
                                                        userHeader,      & ! IUHEAD   -> user header array
                                                        userHeaderLen,   & ! NUHEAD   -> number of user header elements to store
                                                        0,               & ! IPLAN    -> storage plan (0=always store, 1=only create new, 2=only overwrite existing)
                                                        status)            ! ISTAT   <-  status (0=success) 
                                                else
                                                    !--------!
                                                    ! floats !
                                                    !--------!
                                                    call zspd(           & 
                                                        ifltab,          & ! IFLTAB  <-> file table
                                                        pathnames(o,n),  & ! CPATH    -> record name
                                                        numberOrdinates, & ! NORD     -> number of ordinates
                                                        numberCurves,    & ! NCURVE   -> number of curves
                                                        1,               & ! IHORIZ   -> which var plots on horizontals axis (1=ordinates, 2=values)
                                                        c1unit,          & ! C1UNIT  <-> unit of ordinates  (may be modified to remove current datum)
                                                        type,            & ! C1TYPE   -> data type of ordinates
                                                        c2unit,          & ! C2UNIT  <-> unit of curve values  (may be modified to remove current datum)
                                                        type,            & ! C2TYPE   -> data type of curve values
                                                        fvals,           & ! SVALUES  -> 1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                        '',              & ! CLABEL   -> array of labels for each curve
                                                        .false.,         & ! LABEL    -> whether to store labels
                                                        userHeader,      & ! IUHEAD   -> user header array
                                                        userHeaderLen,   & ! NUHEAD   -> number of user header elements to store
                                                        0,               & ! IPLAN    -> storage plan (0=always store, 1=only create new, 2=only overwrite existing)
                                                        status)            ! ISTAT   <-  status (0=success) 
                                                end if
                                                call zset('VDOW', '', 0)
                                                call assert((status == 0) .eqv. expectSuccess)
                                            end if
                                        end if
                                        call zclose(ifltab)
                                        if (status == 0) then
                                            !------------------------------------------------------------!
                                            ! set the default vertical datum to the datum we stored with !
                                            !------------------------------------------------------------!
                                            call zset('VDTM', currentVerticalDatums(kk), 0)
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
                                                    ifltab,           & ! IFLTAB  <-> file table
                                                    pathnames(o,n),   & ! CPATH    -> record name
                                                    numberOrdinates,  & ! NORD    <-  number of ordinates
                                                    numberCurves,     & ! NCURVE  <-  number of curves
                                                    ihoriz,           & ! IHORIZ  <-  which var plots on horizontals axis (1=ordinates, 2=values)
                                                    c1unit,           & ! C1UNIT  <-  unit of ordinates
                                                    c1type,           & ! C1TYPE  <-  data type of ordinates
                                                    c2unit,           & ! C2UNIT  <-  unit of curve values
                                                    c2type,           & ! C2TYPE  <-  data type of curve values
                                                    dvals_out,        & ! DVALUES <-  1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                    size(dvals),      & ! KVALS    -> size of dvals_out must be at least (ncurve+1) * nords
                                                    nvals,            & ! NVALS   <-  number of ordinates + curve values retrieved
                                                    clabel,           & ! CLABEL  <-  array of labels for each curve
                                                    0,                & ! KLABEL   -> max number of labels to retrieve
                                                    l_label,          & ! LABEL   <-  whether labels were retrieved
                                                    userHeader,       & ! IUHEAD  <-  user header array
                                                    size(userHeader), & ! KUHEAD   -> max number of user header elements to retrieve
                                                    userHeaderLen,    & ! NUHEAD  <-  number of user header elements retrieved
                                                    status)             ! ISTAT   <-  status (0=success)
                                            else
                                                !--------!
                                                ! floats !
                                                !--------!
                                                fvals(1:6) = fordinates(:,l)
                                                fvals(7:12) = fvalues(:,l)
                                                call zrpd(            &
                                                    ifltab,           & ! IFLTAB  <-> file table
                                                    pathnames(o,n),   & ! CPATH    -> record name
                                                    numberOrdinates,  & ! NORD    <-  number of ordinates
                                                    numberCurves,     & ! NCURVE  <-  number of curves
                                                    ihoriz,           & ! IHORIZ  <-  which var plots on horizontals axis (1=ordinates, 2=values)
                                                    c1unit,           & ! C1UNIT  <-  unit of ordinates
                                                    c1type,           & ! C1TYPE  <-  data type of ordinates
                                                    c2unit,           & ! C2UNIT  <-  unit of curve values
                                                    c2type,           & ! C2TYPE  <-  data type of curve values
                                                    fvals_out,        & ! SVALUES <-  1D array of ordinates, 1st curve values, 2nd curve values, ...
                                                    size(fvals),      & ! KVALS    -> size of dvals_out must be at least (ncurve+1) * nords
                                                    nvals,            & ! NVALS   <-  number of ordinates + curve values retrieved
                                                    clabel,           & ! CLABEL  <-  array of labels for each curve
                                                    0,                & ! KLABEL   -> max number of labels to retrieve
                                                    l_label,          & ! LABEL   <-  whether labels were retrieved
                                                    userHeader,       & ! IUHEAD  <-  user header array
                                                    size(userHeader), & ! KUHEAD   -> max number of user header elements to retrieve
                                                    userHeaderLen,    & ! NUHEAD  <-  number of user header elements retrieved
                                                    status)             ! ISTAT   <-  status (0=success)
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
    end do
    write(0,'(/,/,i4,a/,/)') count,' paried data tests passed'
    return
end subroutine testStoreRetrievePairedData

subroutine assert(logical_test)
    logical :: logical_test
    if (.not.logical_test) then
        call abort
    end if
end subroutine assert
