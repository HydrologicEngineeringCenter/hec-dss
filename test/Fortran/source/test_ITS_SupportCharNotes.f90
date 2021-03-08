    SUBROUTINE test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
    itimes1, itimes2, itimes3, itimes4, &
    data1, data2, data3, data4, &
    quality1, quality2, quality3, quality4, &
    cnotes1, cnotes2, cnotes3, cnotes4, &
    cpart, label, location, size, totalSize, checkAsending)
!
!    Support function for testing of irregular interval time series functions
!    
!   There are numerous perpitrations for irregular interval data,
!   so testing is quite complex (way more so than the code), and
!   there is no guarantee that all will be covered.
!           
      implicit none
!      
      integer messageUnit, status, location, size, totalSize, storeFlag
      integer(8) ifltab1(*)
      logical checkAsending
!
      integer itimes1(1010), itimes2(1010), itimes3(1010), itimes4(1010), indx(1010)
      double precision data1(2200), data2(2200), data3(2200), data4(2200)   
      integer quality1(2,2200), notes1(4,2200)
      integer quality2(2,2200), notes2(4,2200)
      integer quality3(2,2200)
      integer quality4(2,2200)
      character cnotes1(2200)*50
      character cnotes2(2200)*50
      character cnotes3(2200)*50
      character cnotes4(2200)*50
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12, label*(*), cpart*(*)
      integer nvals, i, origStart, ipos, nprimary 
      integer baseDate, nindx, jvals
      integer startData
      integer notes(1)
      integer kvals, zero, precision
      integer lenQuality
      integer length
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer granularity, readFlag
      integer userHeader(1)
      double precision coordinates(3)
      integer icoord(6)
      integer timeGranularitySeconds

      common /lchk/ lcheck
      logical lcheck
!
!
  
   startData = 200
   baseDate = 0
   origStart = startData
   !  Write primary data set
   call chrlnb(cpart, i)   
    cpath1 = '/Irregular Interval Char Notes/block test/' //  cpart(1:i) // '/date/~1Hour/' // label // '/'
    jvals = 100
    nprimary = jvals
    
       !Order times so that we can more easily check for valid values
    call orderTimes(itimes1(startData), jvals, itimes4(location), size, storeFlag, indx, nindx)
    if (nindx.lt.1) then
        write(messageUnit,*)'Error in orderTimes'
        go to 900
    endif
    

    !j = itimes1(3) / 1440
    !min = itimes1(3) - (j * 1440)
    !call juldat(j, 104, cd, nd)
    !call m2ihm(min, ct)
    !write(messageUnit,*)' Date is ', cd(1:nd), ',  time is ', ct
    

    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1(startData), jvals, &
    data1(startData), 2, quality1, 2, notes1, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    !   Verify
     kvals = 2200
    lenQuality = 2
    readFlag = 0
    zero = 0
    
    call ztsIrregRetClear(ifltab1, cpath1, '01JAN1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '01JAN1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2(startData), granularity, baseDate, data2(startData), 2, lenValuesRead, quality2, lenQuality, &
    lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)
     
    write (*,*)'nvals read = ',nvals
     
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(jvals, nvals, 'test_ITS_SupportCharNotes primary verify number stored, loc 10  ' // label, status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_SupportCharNotes primary verify lenValuesRead stored, loc 10  '  // label, status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_SupportCharNotes primary verify lenQualityRead stored, loc 10  ' // label, status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_SupportCharNotes primary verify lenNotesRead stored, loc 10  ' // label, status)
    if (status.ne.0) go to 900
    call checkNumbers(1400,totalNotesRead, 'test_ITS_SupportCharNotes primary verify totalNotesRead stored, loc 10  ' &
      // label, status)
    if (status.ne.0) go to 900
    call checkTimes(itimes1(startData), itimes2(startData), baseDate, granularity, nvals, &
    'test_ITS_SupportCharNotes primary verify times, loc 10  ' // label, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_SupportCharNotes primary verify units, loc 10  ' // label, status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_SupportCharNotes primary verify type, loc 10  ' // label, status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_SupportCharNotes primary verify precision, loc 10  ' // label, status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(startData), data2(startData), nvals, 'test_ITS_SupportCharNotes primary verify Values, loc 10  ', &
     status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQualityRead, nvals, 'test_ITS_SupportCharNotes primary verify Quality, loc 10  ' &
    // label, status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_SupportCharNotes primary verify cNotes, loc 10  ' // label, status)
    if (status.ne.0) go to 900 

    

    !  Write secondary data set
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes4(location), size, &
    data4(location), 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 3, '', storeFlag, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
   
    !   Verify
      call ztsIrregRetClear(ifltab1, cpath1, '01JAN1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '01JAN1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)
     
    
 
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900 
    if (storeFlag.eq.0) then
        call checkNumbers(nindx, nvals, 'test_ITS_SupportCharNotes primary verify number stored, loc 20a ' // label, status)
    else
        call checkNumbers(totalSize, nvals, 'test_ITS_SupportCharNotes primary verify number stored, loc 20b ' // label, status)
    endif
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_SupportCharNotes primary verify lenValuesRead stored, loc 20 '  // label, status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_SupportCharNotes primary verify lenQualityRead stored, loc 20 ' // label, status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_SupportCharNotes primary verify lenNotesRead stored, loc 20 ' // label, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_SupportCharNotes primary verify units, loc 20 ' // label, status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_SupportCharNotes primary verify type, loc 20 ' // label, status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_ITS_SupportCharNotes primary verify precision, loc 20  ' // label, status)
    if (status.ne.0) go to 900


    
    if (checkAsending) then
    do 110 i=2,nvals
        if (itimes2(i).le.itimes2(i-1)) then
            write (*,*)'Times not acending in function test_ITS_Full secondary verify ', label
            write (*,*)i, itimes2(i), itimes2(i-1)
            go to 900
        endif
        if (data2(i).le.data2(i-1)) then
            write (*,*)'Data not acending in function test_ITS_Full secondary verify ', label
            write (*,*)i, data2(i), data2(i-1)
            go to 900
        endif
110 continue
    endif 
    
       do 200 i=1,nvals
        ipos = indx(i)
        if (ipos.gt.nprimary) then
           ipos = ipos - nprimary + location -1
           call checkDoubles(data4(ipos), data2(i), 1, 'test_ITS_SupportCharNotes Values, loc 30 ' // label, status)
           if (status.ne.0) go to 900
           call checkTimes(itimes4(ipos), itimes2(i), baseDate, granularity, 1, 'test_ITS_SupportCharNotes times, loc 30 ' &
           // label, status)
           if (status.ne.0) go to 900
        else
           ipos = ipos + origStart -1
           call checkDoubles(data1(ipos), data2(i), 1, 'test_ITS_SupportCharNotes Values, loc 40 ' // label, status)
           if (status.ne.0) go to 900
           call checkTimes(itimes1(ipos), itimes2(i), baseDate, granularity, 1, 'test_ITS_SupportCharNotes times, loc 40 ' &
            // label, status)
           if (status.ne.0) go to 900
        endif
 200   continue    
    
!
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810) label
 810        Format('Irregular Time Series Passed Successfully: ', a)  
        else
            Write(messageUnit, 820) label
 820        Format('File Check FAILED in Irregular Time Series: ', a)   
        endif         
       return
!
900 Continue
    call zclose(ifltab1)
        write(messageUnit, 1) label
1       Format('Irregular Time Series test FAILED: ', A)
        write(messageUnit, 901)cpath1
 901    format('Path = ',a)         
 910    Continue
        if (status.eq.0) status = -1
        return
        end