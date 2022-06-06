    SUBROUTINE test_ITS_Support(ifltab1, messageUnit, status, storeFlag, &
    itimes1, itimes2, itimes3, itimes4, &
    data1, data2, data3, data4, &
    label, secondayIndex, secondaySize, expectedTotalSize, checkAsending)

!
!    Support function for testing of irregular interval time series functions
    
!    this method works by reading and writing time series data twice to the same path
!     
!     1) The first write uses data starting at index = 200 (startData) and writes 100(nvals) values
!     2) Next itimes4/data4 is written starting at secondayIndex, and secondarySize
!   
!    
!   There are numerous perpitrations for irregular interval data,
!   so testing is quite complex (way more so than the code), and
!   there is no guarantee that all will be covered.
!           
      implicit none
!      
      integer messageUnit ! unit for output messages
      integer  status     ! output status (non-zero indicates error)
      integer secondayIndex    ! index used to write secondary data set. 
      integer secondaySize        ! number of values to store in secondary data set. (note: primary set has 100) 
      integer expectedTotalSize   ! total expected size after writing secondary data set. 
      integer storeFlag   ! if storeFlag=1 replace existing data, if storeFlag =0 merge
      integer(8) ifltab1(*)
      logical checkAsending ! when true verify data2 and times2 are in ascending order.
!
      integer itimes1(1010), itimes2(2010), itimes3(1010), itimes4(1010)
      real data1(1010) ! input data set array
      real data2(2010) ! array used for reading data
      real data3(1010) ! copy of data1- used to compare to data read into data2 
      real data4(1010) ! copy of data- use this 'un-used' array for writing secondary data set.
      
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12, label*(*)
      integer nvals, i  
      integer JULS, ISTIME, JULE, IETIME
      integer baseDate
      integer startData

      common /lchk/ lcheck
      logical lcheck
!
!
  
   startData = 200
   !  Write primary data set   
    cpath1 = '/Irregular Interval Time Series/block test/Flow/date/~1Hour/' // label // '/'
    nvals = 100
    baseDate = 0
    
    call SaveToTextFile('dump_primary_full'// label //'.txt',cpath1,itimes1,data1,1000,1440)
    
    call SaveToTextFile('dump_primary'// label //'.txt',cpath1,itimes1(startData),data1(startData),nvals,1440)
    
    call zsits(ifltab1, cpath1, itimes1(startData), data1(startData), nvals, baseDate, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    !   Verify
    call datjul("01Sep1955", juls, status)
    jule = juls + 29  
    istime = 1
    ietime = 1440
    baseDate = 0 
    
    call zrits(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, itimes2(startData), data2(startData), 1000, &
     NVALS, baseDate, cunits, ctype, status)   
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
     call checkNumbers(100, nvals, 'test_ITS_Full primary verify number stored, loc 100 ' // label, status)
    if (status.ne.0) go to 900
     call checkInts(itimes3(startData), itimes2(startData), 1, NVALS, 'test_ITS_Full primary verify times, loc 100 ' &
     // label, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Full primary verify units, loc 100 ' // label, status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Full primary verify type, loc 100 ' // label, status)
    if (status.ne.0) go to 900
    call checkFloats(data3(startData), data2(startData), nvals, 'test_ITS_Full primary verify Values, loc 100 ' // label, status)
    if (status.ne.0) go to 900

    !  Write secondary data set
    if (secondayIndex < startData) startData = secondayIndex
    nvals = secondaySize
    baseDate = 0
    call saveToTextFile('dump_secondary'// label // '.txt',cpath1,itimes4(secondayIndex),data4(secondayIndex),nvals,1440)
    call zsits(ifltab1, cpath1, itimes4(secondayIndex), data4(secondayIndex), nvals, baseDate, 'CFS', 'PER-AVER', storeFlag, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    !   Verify
    
    call datjul("01Sep1955", juls, status)
    jule = juls + 29  
    istime = 1
    ietime = 1440
    baseDate = 0 
 !  call zset('mlvl', ' ', 12)
    call zrits(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, itimes2(startData), data2(startData), 1000, NVALS, &
        baseDate, cunits, ctype, status)   
    if (status.ne.0) go to 900
    
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
     call checkNumbers(expectedTotalSize, nvals, 'test_ITS_Full secondary verify number stored, loc 200 ' // label, status)
    if (status.ne.0) go to 900
     
    call checkString('CFS', cunits, 'test_ITS_Full secondary verify units, loc 200 ' // label, status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Full secondary verify type, loc 200 ' // label, status)
    if (status.ne.0) go to 900
    
    if (checkAsending) then
    do i=startData+1,nvals
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
    end do
    else
        call checkInts(itimes4(startData), itimes2(startData), 1, NVALS, 'test_ITS_Full secondary verify times, loc 300 ' &
          // label, status)
        if (status.ne.0) go to 900
        call checkFloats(data4(startData), data2(startData), nvals, 'test_ITS_Full secondary verify Values, loc 300 ' &
         // label, status)
        if (status.ne.0) go to 900
    endif 
    
!
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810) label
 810        Format('Basic Irregular Time Series Passed Successfully: ', a)  
        else
            Write(messageUnit, 820) label
 820        Format('File Check FAILED in Basic Irregular Time Series: ', a)   
        endif         
       return
!
 900    Continue
        write(messageUnit, 1) label
1       Format('Basic Irregular Time Series test FAILED: ', A) 
 910    Continue
        if (status.eq.0) status = -1
        return
        end