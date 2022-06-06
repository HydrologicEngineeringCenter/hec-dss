    SUBROUTINE test_ITS_Full(ifltab1, messageUnit, status)
!
!    Test Full writing and reading of irregular interval time series functions
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
      real x
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12
      integer nvals, i, nread 
      integer JULS, ISTIME, JULE, IETIME
      integer baseDate
      integer j, storeFlag, readFlag
      integer numberVals, lengthEachValue, lengthEachQuality, lengthEachNote
      logical ascending 
      integer totalLengthCNotes
      integer lengthUserHeader
      
      integer idum
      double precision d(2)
      integer quality(2)
      
      character label*11, letter*1

      common /lchk/ lcheck
      logical lcheck
!
!
      WRITE(messageUnit, *)'Begin test_ITS_Full'
      nvals = 1000
      call datjul("06Sep1955", juls, status)
      juls = juls * 1440
      do i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)
       data4(i) = data1(i)
       itimes1(i) = (i * 1440) + juls
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i) 
      end do
 
 
!     call zset('mlvl', ' ', 12)
!
!   First, write a single value and then read it back
 1  Format('Full Irregular Time Series test FAILED')   
    cpath1 = '/Full Irregular Time Series/One Value/Flow/date/~1Day/F/'
    nvals = 1
    baseDate = 0
    call zsits(ifltab1, cpath1, itimes1, data1(2), nvals, baseDate, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    call datjul("06Sep1955", juls, status)
    jule = juls + 2
    juls = juls -2
    istime = 0
    ietime = 0
    baseDate = 0
    NVALS = 1000
    call zrits(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, itimes2, data2, 1010, NVALS, baseDate, cunits, ctype, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(1, nvals, 'test_ITS_Full number stored, loc 5', status)
    if (status.ne.0) go to 900

    call checkInts(itimes3, itimes2, 1, NVALS, 'test_ITS_Full times, loc 5', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Full units, loc 5', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Full type, loc 5', status)
    if (status.ne.0) go to 900
    call checkFloats(data3(2), data2, nvals, 'test_ITS_Full Values, loc 5', status)
    if (status.ne.0) go to 900


    !  Now write 1000 values and check
    cpath1 = '/Full Irregular Time Series/several/Flow/date/~1Day/F/'
    nvals = 1000
    baseDate = 0
   ! itimes1(700) = itimes1(699) - 1
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
  !  call zset('mlvl', ' ', 10)
    call zrits(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, itimes2, data2, 1010, NVALS, baseDate, cunits, ctype, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
     call checkNumbers(1000, nvals, 'test_ITS_Full number stored, loc 10', status)
    if (status.ne.0) go to 900
     call checkInts(itimes3, itimes2, 1, NVALS, 'test_ITS_Full times, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Full units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Full type, loc 10', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_ITS_Full Values, loc 10', status)
    if (status.ne.0) go to 900

    !  Test getNumberValues function
    call ztsGetSizes(ifltab1, cpath1, '25DEC1955', '1400', '04JAN1958', '0800', &
    numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    !  The above call is for ALL values in the records within time range.
    call checkNumbers(1000, numberVals, 'test_ITS_Full ztsGetSizes number stored, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, lengthEachValue, 'test_ITS_Full ztsGetSizes lengthEachValue, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(0, lengthEachQuality, 'test_ITS_Full ztsGetSizes lengthEachQuality, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(0, lengthEachNote, 'test_ITS_Full ztsGetSizes lengthEachNote, loc 11', status)
    if (status.ne.0) go to 900
  
    
    !Re-write in place
    cpath1 = '/Full Irregular Time Series/several/Flow/date/~1Day/F/'
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
     call checkNumbers(1000, nvals, 'test_ITS_Full number stored, loc 20', status)
    if (status.ne.0) go to 900
     call checkInts(itimes3, itimes2, 1, NVALS, 'test_ITS_Full times, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Full units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Full type, loc 20', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_ITS_Full Values, loc 20', status)
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
    call checkFloats(vals, data2, 1, 'test_ITS_Full Values, loc 21', status)
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
    call checkFloats(vals, data2, 1, 'test_ITS_Full Values, loc 22', status)
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
    call checkFloats(vals, data2, 1, 'test_ITS_Full Values, loc 24', status)
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
    call checkFloats(vals, data2, 1, 'test_ITS_Full Values, loc 25', status)
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
    call checkFloats(vals, data2, 1, 'test_ITS_Full Values, loc 26', status)
    if (status.ne.0) go to 900
    vals(1) = 484.0
    data2(1) = data2(NVALS)
    call checkFloats(vals, data2, 1, 'test_ITS_Full Values, loc 27', status)
    if (status.ne.0) go to 900
    
    


    !offset by 12 hours and rewrite full data set
    nvals = 1000
    do i=1,nvals
        itimes1(i) = itimes1(i) - 720
        itimes3(i) = itimes1(i)
        data1(i) = data1(i) - 0.5
    end do
    cpath1 = '/Full Irregular Time Series/several/Flow/date/~1Day/F/'    
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
     call checkNumbers(2000, nvals, 'test_ITS_Full number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Full units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Full type, loc 30', status)
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
            call checkInts(itimes1, itimes2, 1, 1, 'test_ITS_Full times, loc 30', status)
            if (status.ne.0) go to 900
        endif
        if (data2(i).ne.x) then
            !  Hoky way to get around a compiler error
            data1(1) = x
            data2(1) = data2(i)
            call checkFloats(data1, data2, 1, 'test_ITS_Full Values, loc 30', status)
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
    !   a.  Merge, times on (i.e., same interval)
    !   b.  Replace, times on (i.e., same interval)
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
  
       !   1.  Write a data set before it
       label = 'ITS Full 1' //  letter
       call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 100, 100, 200, ascending)
        if (status.ne.0) go to 900

        !   2.  Write a data set after it
        label = 'ITS Full 2' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 300, 100, 200, ascending)
        if (status.ne.0) go to 900
        
       !   3.  Write a data set that exactly encompasses it
       nread = 100
       if (j.eq.3) nread = 200
       if (j.eq.4) nread = 101
       label = 'ITS Full 3' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 200, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   4.  Write a data set that starts before and ends in block
         nread = 150
        if (j.eq.3) nread = 200
        if (j.eq.4) nread = 151
        label = 'ITS Full 4' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 150, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   5.  Write a data set that starts in the middle and ends after
        nread = 150
        if (j.eq.3) nread = 200
        if (j.eq.4) nread = 150
        label = 'ITS Full 5' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 250, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   6.  Write a data set that starts and ends in the block.
        nread = 100
       if (j.eq.3) nread = 150
       if (j.eq.4) nread = 101
       label = 'ITS Full 6' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 225, 50, nread, ascending)
        if (status.ne.0) go to 900
        
        !   7.  Write a data set that starts before and ends after the block
        nread = 300
       if (j.eq.3) nread = 400
       label = 'ITS Full 7' //  letter
        call test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            label, 100, 300, nread, ascending)
        if (status.ne.0) go to 900
        
    
    end do
!
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Full Irregular Interval Time Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Full Irregular Interval Time Series') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end