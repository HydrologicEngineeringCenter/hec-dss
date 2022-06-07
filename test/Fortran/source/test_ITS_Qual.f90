    SUBROUTINE test_ITS_Qual(ifltab1, messageUnit, status)
!
!    Test basic writing and reading of irregular interval time series functions
!    
!   There are numerous perpitrations for irregular interval data,
!   so testing is quite complex (way more so than the code), and
!   there is no guarantee that all will be covered.
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
      integer itimes1(1010), itimes2(2010), itimes3(1010), itimes4(1010)
      real data1(1010), data2(2010), data3(1010), data4(1010), vals(10)
      integer quality1(2,1010), notes1(4,1010)
      integer quality2(2,1010), notes2(4,1010)
      integer quality3(2,1010), notes3(4,1010)
      character cnotes1(2)*50
      real x
      character cpath1*100 
      character cunits*12, ctype*12, ctemp*12, tzone*30
    
      integer nvals, i, nread 
      integer JULS, ISTIME, JULE, IETIME, minutesInBaseDate
      integer baseDate, timeGranularitySeconds
      integer j, storeFlag, readFlag
      integer numberVals, lengthEachValue, lengthEachQuality, lengthEachNote
      logical ascending 
      integer lengthUserHeader

      integer  kvals, zero
      integer length
      integer numberValuesRead
      integer expectedTotalSize
      integer secondaySize
      integer secondayIndex
!
      integer userHeader(1)
      
      integer idum
      double precision d(2)
      integer quality(2)

      integer totalLengthCNotes
      integer maxVals, precision
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead

      character label*11, letter*1
      

      common /lchk/ lcheck
      logical lcheck
!
!
      WRITE(messageUnit, *)'Begin test_ITS_Qual'
      nvals = 1000
      call datjul("06Sep1955", juls, status)
      minutesInBaseDate = juls * 1440
      do i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)
       data4(i) = data1(i)
       itimes1(i) = (i * 1440) + minutesInBaseDate
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i) 
      end do

    do i=1, nvals
        do j=1,2
        quality1(j,i) = (i*100) + j
        quality3(j,i) = quality1(j,i) 
        end do
        do j=1,4
        notes1(j,i) = (i*10000) + j
        notes3(j,i) = notes1(j,i)        
        end do
    end do

 
     !call zset('mlvl', ' ', 8)
!
 1  Format('Basic Irregular Time Series test FAILED')   
   

   
    !  Now write 1000 values and check
    cpath1 = '/Irregular Time Series/Quality and Notes/Flow/date/~1Day/F/'
    kvals = 1000
    timeGranularitySeconds = 60
    call ztsirregstorefull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, kvals, &
    data1, 1, quality1, 2, notes1, 4, cnotes1, 0, userHeader, 0, &  
    'CFS', 'PER-AVER', 1, 'Pacific Time', 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
 
    baseDate = 0
    timeGranularitySeconds = 60
    maxVals = 1010 
    zero = 0 
    call ztsIrregRetrieveFull(ifltab1, cpath1, '04Sep1955', '2400', '04Dec1958', '2400', maxVals, numberValuesRead, &
    itimes2, timeGranularitySeconds, baseDate, data2, 1, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes1, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, tzone, 0, status) 

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900


    call checkString('Pacific Time', tzone, 'test_ITS_Qual time zone name, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,numberValuesRead, 'test_ITS_Qual number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Qual units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Qual type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_ITS_Qual number precision, loc 10', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_ITS_Qual Values, loc 10', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_ITS_Qual Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_ITS_Qual Notes, loc 10', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, timeGranularitySeconds, numberValuesRead, 'test_ITS_Qual times, loc 10', status)
    if (status.ne.0) go to 900
  

    !  Test getNumberValues function
    call ztsGetSizes(ifltab1, cpath1, '25DEC1955', '1400', '04JAN1958', '0800', &
    numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    !  The above call is for ALL values in the records within time range.
    call checkNumbers(1000, numberVals, 'test_ITS_Qual ztsGetSizes number stored, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, lengthEachValue, 'test_ITS_Qual ztsGetSizes lengthEachValue, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachQuality, 'test_ITS_Qual ztsGetSizes lengthEachQuality, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, lengthEachNote, 'test_ITS_Qual ztsGetSizes lengthEachNote, loc 11', status)
    if (status.ne.0) go to 900


     !  All values
    call ztsGetSizes(ifltab1, cpath1, '04Sep1955', '2400', '04Dec1958', '2400', &
    numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    !  The above call is for ALL values in the records within time range.
    call checkNumbers(1000, numberVals, 'test_ITS_Qual ztsGetSizes number stored, loc 12', status)
    if (status.ne.0) go to 900

    !  crossing records
    call ztsGetSizes(ifltab1, cpath1, '30DEC1955', '2400', '02JAN1956', '2400', &
    numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    !  The above call is for ALL values in the records within time range.
    call checkNumbers(483, numberVals, 'test_ITS_Qual ztsGetSizes number stored, loc 15', status)
    if (status.ne.0) go to 900

  
  !  FIX ME - do some of these tests with quality and notes!
    
    !Re-write in place
    cpath1 = '/Irregular Time Series/Quality and Notes/Flow/date/~1Day/F/'
    nvals = 1000
    baseDate = 0
    call zsits(ifltab1, cpath1, itimes1, data1, nvals, baseDate, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    call datjul("06Sep1955", juls, status)
    jule = juls + 1020
    juls = juls -2
    istime = 0
    ietime = 0
    baseDate = 0
    NVALS = 1010
    call zrits(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, itimes2, data2, 1010, NVALS, baseDate, cunits, ctype, status)
   
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
     call checkNumbers(1000, nvals, 'test_ITS_Qual number stored, loc 20', status)
    if (status.ne.0) go to 900
     call checkInts(itimes3, itimes2, 1, NVALS, 'test_ITS_Qual times, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Qual units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Qual type, loc 20', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_ITS_Qual Values, loc 20', status)
    if (status.ne.0) go to 900
    	
    !  Read data getting previous value in same block
    call datjul("01Jan1956", juls, status)
    juls = juls + 1
    jule = juls + 1    
    istime = 1440
    ietime = 0
    baseDate = 0
    NVALS = 1010
    !  First value should be for 01 Jan 1956, 2400 (not 02 Jan)
    readFlag = 1
    call zritsi(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, .false., idum, &
    itimes2, data2, d, 1010, NVALS, baseDate, quality, .false., idum, &
    cunits, ctype, quality, .false., idum,  d, quality, idum, readFlag, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    vals(1) = 118.0
    call checkFloats(vals, data2, 1, 'test_ITS_Qual Values, loc 21', status)
    if (status.ne.0) go to 900
    
    !  Read data getting previous value in previous block (31Dec1955)
    juls = juls -1
    readFlag = 1
    call zritsi(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, .false., idum, &
    itimes2, data2, d, 1010, NVALS, baseDate, quality, .false., idum, &
    cunits, ctype, quality, .false., idum,  d, quality, idum, readFlag, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    vals(1) = 117.0
    call checkFloats(vals, data2, 1, 'test_ITS_Qual Values, loc 22', status)
    if (status.ne.0) go to 900

    !   Now subsequent value in same block
    call datjul("30Dec1956", jule, status)
    juls = jule - 4    
    istime = 1440
    ietime = 1440
    baseDate = 0
    NVALS = 1010
    !   value should be for 31 Dec 1956, 2400 
    readFlag = 2
    call zritsi(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, .false., idum, &
    itimes2, data2, d, 1010, NVALS, baseDate, quality, .false., idum, &
    cunits, ctype, quality, .false., idum,  d, quality, idum, readFlag, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    vals(1) = 483.0
    data2(1) = data2(NVALS)
    call checkFloats(vals, data2, 1, 'test_ITS_Qual Values, loc 24', status)
    if (status.ne.0) go to 900

    !   Subsequent value in next block
    call datjul("31Dec1956", jule, status)
    juls = jule - 4    
    istime = 1440
    ietime = 1440
    baseDate = 0
    NVALS = 1010
    !   value should be for 01 Jan 1957, 2400 
    readFlag = 2
    call zritsi(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, .false., idum, &
    itimes2, data2, d, 1010, NVALS, baseDate, quality, .false., idum, &
    cunits, ctype, quality, .false., idum,  d, quality, idum, readFlag, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    vals(1) = 484.0
    data2(1) = data2(NVALS)
    call checkFloats(vals, data2, 1, 'test_ITS_Qual Values, loc 25', status)
    if (status.ne.0) go to 900

     !   Now previous and subsequent value in differnt blocks
    call datjul("01Jan1956", juls, status)
    call datjul("31Dec1956", jule, status)  
    istime = 1440
    ietime = 1440
    baseDate = 0
    NVALS = 1010   
    readFlag = 3
    call zritsi(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, .false., idum, &
    itimes2, data2, d, 1010, NVALS, baseDate, quality, .false., idum, &
    cunits, ctype, quality, .false., idum,  d, quality, idum, readFlag, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    vals(1) = 117.0  
    call checkFloats(vals, data2, 1, 'test_ITS_Qual Values, loc 26', status)
    if (status.ne.0) go to 900
    vals(1) = 484.0
    data2(1) = data2(NVALS)
    call checkFloats(vals, data2, 1, 'test_ITS_Qual Values, loc 27', status)
    if (status.ne.0) go to 900
    
    


    !offset by 12 hours and rewrite full data set
    nvals = 1000
    do i=1,nvals
        itimes1(i) = itimes1(i) - 720
        itimes3(i) = itimes1(i)
        data1(i) = data1(i) - 0.5
    end do
    baseDate = 0    
    call zsits(ifltab1, cpath1, itimes1, data1, nvals, baseDate, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    NVALS = 1010
    call datjul("06Sep1955", juls, status)
    jule = juls + 1020
    juls = juls -2
    istime = 0
    ietime = 0
    baseDate = 0
    NVALS = 1010
    call zrits(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, itimes2, data2, 2000, NVALS, baseDate, cunits, ctype, status)
   
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    !  1 extra since we are overwritting 12 hours before
     call checkNumbers(2000, nvals, 'test_ITS_Qual number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Qual units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Qual type, loc 30', status)
    if (status.ne.0) go to 900
    !
    !  Since data merged, we need to check times and values in a loop
    istime = itimes1(1)
    x = 0.5
    do i=1,nvals
        if (itimes2(i).ne.istime) then
            !  Hoky way to get around a compiler error
            itimes1(1) = istime
            itimes2(1) = itimes2(i)
            call checkInts(itimes1, itimes2, 1, 1, 'test_ITS_Qual times, loc 30', status)
            if (status.ne.0) go to 900
        endif
        if (data2(i).ne.x) then
            !  Hoky way to get around a compiler error
            data1(1) = x
            data2(1) = data2(i)
            call checkFloats(data1, data2, 1, 'test_ITS_Qual Values, loc 30', status)
            if (status.ne.0) go to 900
        endif
        x = x + 0.5
        istime = istime + 720
    end do



    !  Now we will write a data set in the middle of a record (block), then
    !   1.  Write a data set before it
    !   2.  Wrtie a data set after it
    !   3.  Write a data set that exactly encompasses it
    !   4.  Write a data set that starts before and ends in block
    !   5.  Write a data set that starts in the middle and ends after    
    !   6.  Write a data set that starts and ends in the block.
    !   7.  Write a data set that starts before and ends after the block

    !  This is done with 4 perturbations:
    !   a.  Merge, times on (e.g., same interval)
    !   b.  Replace, times on (e.g., same interval)
    !   c.  Merge, times off
    !   d.  Replace, times off
    

    !for this data, let's use quasi-hourly data (ir-month).  Easier to test within a month block
    nvals = 1000
    call datjul("06Sep1955", juls, status)
    juls = juls * 1440
    do i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)
       data4(i) = data1(i)
       itimes1(i) = (i * 60) + juls
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i) 
    end do
 
    ascending = .false.
    do j=1,4
    
        if (j.eq.1) then
            letter = 'a'
            storeFlag = 0
        else if (j.eq.2) then
            letter = 'b'
            storeFlag = 1  
!            call zset('mlvl', '', 10)          
        else if (j.eq.3) then
            letter = 'c'
            storeFlag = 0
        else if (j.eq.4) then
            letter = 'd'
            storeFlag = 1
        endif 
        
        !  If we are at loop 3, we need to use offset times.  
        !  Rebuild the time array by 30 mins off
        if (j.eq.3) then
            do i=1, nvals
               itimes4(i) = itimes1(i) - 30 
               data4(i) = data1(i) - 0.5             
            end do
            !  With the offset, it would be hard to compute all the
            !  results arrays (we'd have to fix gaps), but we know
            !  that the times and data are ascending, so just check that
            !  Almost any error would cause an order issue
            ascending = .true.
        endif
        
        !  label, location, size, totalSize, checkAsending)
        expectedTotalSize = 200
        secondaySize = 100
        secondayIndex = 100
       !   1.  Write a data set before it
       label = 'ITS Qual 1' //  letter
       call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, secondayIndex, secondaySize, expectedTotalSize, ascending)
        if (status.ne.0) go to 900

        !   2.  Write a data set after it
        label = 'ITS Qual 2' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 300, 100, 200, ascending)
        if (status.ne.0) go to 900
        
       !   3.  Write a data set that exactly encompasses it
       nread = 100
       if (j.eq.3) nread = 200
       if (j.eq.4) nread = 101
       label = 'ITS Qual 3' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 200, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   4.  Write a data set that starts before and ends in block
         nread = 150
        if (j.eq.3) nread = 200
        if (j.eq.4) nread = 151
        label = 'ITS Qual 4' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 150, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   5.  Write a data set that starts in the middle and ends after
        nread = 150
        if (j.eq.3) nread = 200
        if (j.eq.4) nread = 150
        label = 'ITS Qual 5' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 250, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   6.  Write a data set that starts and ends in the block.
        nread = 100
       if (j.eq.3) nread = 150
       if (j.eq.4) nread = 101
       label = 'ITS Qual 6' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 225, 50, nread, ascending)
        if (status.ne.0) go to 900
        
        !   7.  Write a data set that starts before and ends after the block
        nread = 300
       if (j.eq.3) nread = 400
       label = 'ITS Qual 7' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 100, 300, nread, ascending)
        if (status.ne.0) go to 900
        
    
    end do
!
!
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Basic Regular Time Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Basic Regular Time Series') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 
        if (status.eq.0) status = -1
        return
        end