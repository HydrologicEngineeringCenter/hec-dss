program testVerticalDatum
    call testStoreRetrieveTimeSeries()
    call testStoreRetrievePairedData()

    stop
end program testVerticalDatum

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
        real (kind=8) :: elevation, offsetToNgvd29, offsetToNavd88
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
            elevation,                        &
            offsetNgvd29,                     &
            offsetNgvd29IsEstimate,           &
            offsetNavd88,                     &
            offsetNavd88IsEstimate,           &
            generateCompressed)
            character (len = *),  intent(out) :: outputStr
            character (len = *),  intent(out) :: errorMessage
            character (len = *),  intent(in)  :: nativeDatum
            character (len = *),  intent(in)  :: unit
            real      (kind = 8), intent(in)  :: elevation
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
            elevation,                        &
            offsetNgvd29,                     &
            offsetNgvd29IsEstimate,           &
            offsetNavd88,                     &
            offsetNavd88IsEstimate)
            character (len = *),  intent(in)  :: inputStr
            character (len = *),  intent(out) :: errorMessage
            character (len = *),  intent(out) :: nativeDatum
            character (len = *),  intent(out) :: unit
            real      (kind = 8), intent(out) :: elevation
            real      (kind = 8), intent(out) :: offsetNgvd29
            logical   (kind = 4), intent(out) :: offsetNgvd29IsEstimate
            real      (kind = 8), intent(out) :: offsetNavd88
            logical   (kind = 4), intent(out) :: offsetNavd88IsEstimate
        end subroutine stringToVerticalDatumInfo
    end interface
   
    contains
        subroutine initVerticalDatumInfo(vdi)
            type(verticalDatumInfo) vdi
            vdi%elevation = UNDEFINED_VERTICAL_DATUM_VALUE
            vdi%offsetToNavd88 = UNDEFINED_VERTICAL_DATUM_VALUE
            vdi%offsetToNgvd29 = UNDEFINED_VERTICAL_DATUM_VALUE
            vdi%nativeDatum = ' '
            vdi%offsetToNavd88IsEstimate = .false.
            vdi%offsetToNgvd29IsEstimate = .false.
        end subroutine initVerticalDatumInfo

        integer function byteCountToIntCount(byteCount)
            integer :: byteCount
            byteCountToIntCount = (byteCount-1) / 4 + 1
        end function byteCountToIntCount

    end module modVerticalDatumInfo


subroutine testStoreRetrieveTimeSeries()
    use modVerticalDatumInfo
    implicit none

    integer (kind=8)        :: ifltab(250)
    integer (kind=4)        :: status, numberValues, i, j, k, k2, k3, kk, l, m, n, o, p
    integer (kind=4)        :: quality(6), itimes(6,2),userHeader(100), userHeaderLen
    real (kind=8)           :: dvalues(6,3), dvals(6)
    real (kind=4)           :: fvalues(6,3), fvals(6)
    character (len=300)     :: errmsg, vdiStr
    character (len=80)      :: filename(2)
    character (len=80)      :: pathnames(2,2)
    character (len=16)      :: unit(3), type, verticalDatums(3)
    character (len=32)      :: unitSpec
    character (len=16)      :: startDate(2), endDate(2)
    character (len=4)       :: startTime, endTime
    character (len=400)     :: userHeaderStr
    type(verticalDatumInfo) :: vdi(2)

    integer  :: ivdatum
    character (len=cverticalDatumLen) :: cvdatum

    equivalence (userHeader, userHeaderStr)

    numberValues = 6

    quality = (/0,0,0,0,0,0/)

    itimes = reshape((/                                             &
        64035420, 64035480, 64035540, 64035600, 64035660, 64035720, &
        64080060, 64080120, 64080180, 64080240, 64080300, 64080360  &
        /),shape(itimes))

    dvalues = reshape((/                                   &
        1000.,1001.,1002.,1003.,1004.,1005.,               &
        304.8,305.1048,305.4096,305.7144,306.0192,306.324, &
        1000.,1001.,1002.,1003.,1004.,1005.                &
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

    verticalDatums = (/CVD_NAVD88, CVD_NGVD29, 'Pensacola'/)

    do j = 1, 2
        call initVerticalDatumInfo(vdi(j))
        if (j == 1) then
            vdi(j)%nativeDatum = CVD_NGVD29
            vdi(j)%elevation = 615.2
            vdi(j)%unit = 'ft'
            vdi(j)%offsetToNavd88 = 0.3855
            vdi(j)%offsetToNavd88IsEstimate = .true.
        else
            vdi(j)%nativeDatum = 'Pensacola'
            vdi(j)%elevation = 757
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
    !     k3  mod(k+1, 3 + 1)
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
    ! p = put vertical datum in user header? (false tests correct retrieval
    !     of vertical datum info from location record)
    !     1 = true
    !     2 = false
    call zset('MLVL', '', 1)
    do i = 1, 2
        do j = 1, 2
            call verticalDatumInfoToString(       &
                vdiStr,                           &
                errmsg,                           &
                vdi(j)%nativeDatum,               &
                vdi(j)%unit,                      &
                vdi(j)%elevation,                 &
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
                vdi(j)%elevation,                &
                vdi(j)%offsetToNgvd29,           &
                vdi(j)%offsetToNgvd29IsEstimate, &
                vdi(j)%offsetToNavd88,           &
                vdi(j)%offsetToNavd88IsEstimate)
            call assert(errmsg == ' ')
            do k = 1, 3
                k2 = mod(i, 3) + 1
                k3 = mod(i+1, 3) + 1
                do l = 1, 3
                    do m = 1, 3
                        do n = 1, 2
                            do o = 1, 2
                                do p = 1, 2
                                    ! write(0,*) i, j, k, l, m, n, o, p
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
                                    !------------------------------------------------!
                                    ! add the vertical datum info to the user header !
                                    !------------------------------------------------!
                                    userHeaderStr = VERTICAL_DATUM_INFO_PARAM//':'//vdiStr
                                    unitSpec = unit(l)
                                    if (m > 1) then
                                        if (p == 1) then
                                            !----------------------------------------------------------!
                                            ! override the default vertical datum with the user header !
                                            !----------------------------------------------------------!
                                            kk = k2
                                            write(userHeaderStr,'(7a)')                                       &
                                                VERTICAL_DATUM_INFO_PARAM,':',vdiStr(1:len_trim(vdiStr)),';', &
                                                VERTICAL_DATUM_PARAM,':',verticalDatums(kk)
                                        end if
                                        if (m > 2) then
                                            !--------------------------------------------------------!
                                            ! override default and user header datums with unit spec !
                                            !--------------------------------------------------------!
                                            kk = k3
                                            write(unitSpec,'(5a)')                             &
                                                'U=',unit(l)(1:len_trim(unit(l))),'|', &
                                                'V=',verticalDatums(kk)
                                        end if
                                    end if
                                    !-------------------------------------------------------!
                                    ! store the time series in the specified vertical datum !
                                    !-------------------------------------------------------!
                                    userHeaderLen = byteCountToIntCount(len_trim(userHeaderStr))
                                    if (n == 1) then
                                        if (o == 1) then
                                            !-------------!
                                            ! RTS doubles !
                                            !-------------!
                                            dvals = dvalues(:, l)
                                            call zsrtsxd(               &
                                                ifltab,                 & ! IFLTAB
                                                pathnames(o,n),         & ! CPATH
                                                startDate(p),           & ! CDATE
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
                                            if (status.ne.0) then
                                                write(0,*) 'Status = ',status
                                            end if
                                            call assert(status == 0)
                                        else
                                            !------------!
                                            ! RTS floats !
                                            !------------!
                                            fvals = fvalues(:, l)
                                            call zsrtsx(                &
                                                ifltab,                 & ! IFLTAB
                                                pathnames(o,n),         & ! CPATH
                                                startDate(p),           & ! CDATE
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
                                            if (status.ne.0) then
                                                write(0,*) 'Status = ',status
                                            end if
                                            call assert(status == 0)
                                        end if
                                    else
                                        if (o == 1) then
                                            !-------------!
                                            ! ITS doubles !
                                            !-------------!
                                            dvals = dvalues(:, l)
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
                                            if (status.ne.0) then
                                                write(0,*) 'Status = ',status
                                            end if
                                            call assert(status == 0)
                                        else
                                            !------------!
                                            ! ITS floats !
                                            !------------!
                                            fvals = fvalues(:, l)
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
                                    call zclose(ifltab)
                                end do
                            end do
                        end do
                    end do
                end do
            end do
        end do
    end do
    return
end subroutine testStoreRetrieveTimeSeries

subroutine testStoreRetrievePairedData()
    return
end subroutine testStoreRetrievePairedData

subroutine assert(logical_test)
    logical :: logical_test
    if (.not.logical_test) call abort
end subroutine assert
