    SUBROUTINE test_ITS_CharNotes(ifltab1, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
      integer itimes1(2200), itimes2(2200), itimes3(2200), itimes4(2200)
    !  Write to DSS array 1, read into array 2, compare with array 3
      !double precision data1(1010), data2(1010), data3(1010)
      double precision data1(2200), data2(2200), data3(2200), data4(2200)   
      integer quality1(2,2200), notes1(4,2200)
      integer quality2(2,2200), notes2(4,2200)
      integer quality3(2,2200), notes3(4,2200)
      integer quality4(2,2200)
      character cnotes1(2200)*50
      character cnotes2(2200)*50
      character cnotes3(2200)*50
      character cnotes4(2200)*50
      integer notes(1)
      character cpath1*100, cpath2*100
      character cunits*12, ctype*12, ctemp*12, tzone*25
      integer ierr
      integer JULS, ISTIME, JULE, IETIME
      integer baseDate
      integer nvals, kvals, i, j, iofset, max, zero
      integer lenQuality, lenNotes, n2, n3 
      integer length, offset, lenCoord, lenIcord
      integer numberValuesRead
      integer numberVals, lengthEachValue, lengthEachQuality, lengthEachNote
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer totalLengthCNotes, granularity
      integer maxVals, precision
      logical lowerCase
      integer lengthUserHeader
      integer timeGranularitySeconds
!
      integer userHeader(1)
      double precision coordinates(3), rcoordinates(3)
      integer icoord(6), ricoord(6) 
      integer k,n,ich 

      common /lchk/ lcheck
      logical lcheck       
      
      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_ITS_CharNotes basic'   
      lenQuality = 2
      lenNotes = 0
      maxVals = 1010

      nvals = 1000
      call datjul('06Sep1955', juls, status)
      lowercase = .false.
      juls = juls * 1440
      ich = ichar('A')
      do i=1, nvals
       itimes1(i) = (i * 1440) + juls
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i) 
       data1(i) =FLOAT(i)
       data3(i) = data1(i)
       cnotes1(i) = ' '
       n = mod(i-1,25) + 1
       if (n.eq.1) lowerCase = .not.lowerCase
       if (lowerCase) then
       do k=1,n
          cnotes1(i)(k:k) = char(ich+k-1+32)          
       end do
       else
       do k=1,n
          cnotes1(i)(k:k) = char(ich+k-1)          
       end do
       endif       
       do j=1,2
        quality1(j,i) = i+j
        quality3(j,i) = quality1(j,i)
      end do
      
      end do
    kvals = nvals
!
 1  Format('Time Series Notes test FAILED')   
   !call zset('mlvl', ' ', 5)
!
! 
    cpath1 = '/Time Series With Notes/Basic 1/Flow/date/IR-YEAR/F/'

   ! cnotes1(1) = ' '
   ! cnotes1(2) = 'Gage was damaged during car accident'
   ! cnotes1(3) = 'George fixed, but not calibrated'
   ! cnotes1(4) = 'Frank calibrated'
   ! cnotes1(5) = ' '
    
    do i=1,nvals
      cnotes3(i) = cnotes1(i)
    end do
    
    nvals = 100
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0, status)

    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    kvals = nvals
 
    zero = 0
    call ztsIrregRetClear(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, 0, status)


    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals, nvals, 'test_ITS_CharNotes Basic number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1400,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotes Basic precision stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 10', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 10', status)
    if (status.ne.0) go to 900 
    
    
    !  Now merge with 1000 values   
    nvals = 1000  
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0, status)

    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    kvals = nvals  
    zero = 0
    call ztsIrregRetClear(ifltab1, cpath1, '01Sep1955', '2400', '24Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Sep1955', '2400', '24Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_ITS_CharNotes Basic number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(14000,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotes Basic precision stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 20', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 20', status)
    if (status.ne.0) go to 900  
    
    !  Now merge with the data off by 12 hours
    do i=1,1000
       itimes1(i) = itimes1(i) - 720
    end do

    nvals = 1000    
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 3, '', 0, status)

    
    !  Checking is difficult, as now we have 2000 values....
    do i=1,1000
        j = i*2 -1
        itimes4(j) = itimes1(i)        
        itimes4(j+1) = itimes1(i) + 720
        data4(j) = data1(i)
        data4(j+1) = data1(i)
        quality4(1,j) = quality1(1,i)
        quality4(2,j) = quality1(2,i)
        quality4(1,j+1) = quality1(1,i)
        quality4(2,j+1) = quality1(2,i) 
        cnotes4(j) = cnotes1(i)
        cnotes4(j+1) = cnotes1(i)       
    end do
    
    
    kvals = 2100
    zero = 0
    call ztsIrregRetClear(ifltab1, cpath1, '01Sep1955', '2400', '24Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Sep1955', '2400', '24Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(2000, nvals, 'test_ITS_CharNotes Basic number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(28000,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_ITS_CharNotes Basic precision stored, loc 30', status)
    if (status.ne.0) go to 900
!    do i=2,125
!	  write(*,*)'debug ',i,itimes4(i), itimes2(i), itimes4(i)-itimes4(i-1),itimes2(i)-itimes2(i-1)
!    end do
    call checkTimes(itimes4, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Time, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data4, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(quality4, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 30', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes4, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 30', status)
    if (status.ne.0) go to 900  
    
   !  Check getNumberValues function
     call ztsGetSizes(ifltab1, cpath1, '01Sep1955', '2400', '24Dec1958', '2400', &
     numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(2000, numberVals, 'test_ITS_CharNotes Basic ztsGetSizes number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachValue, 'test_ITS_CharNotes Basic ztsGetSizes lengthEachValue, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachQuality, 'test_ITS_CharNotes Basic ztsGetSizes lengthEachQuality, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0, lengthEachNote, 'test_ITS_CharNotes Basic ztsGetSizes lengthEachNote, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(28000, totalLengthCNotes, 'test_ITS_CharNotes Basic ztsGetSizes totalLengthCNotes, loc 40', status)
    if (status.ne.0) go to 900
    

    !  Now REPLACE with 1000 values   
    nvals = 1000    
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 4, '', 1, status)

    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    kvals = 2000  
    zero = 0
    call ztsIrregRetClear(ifltab1, cpath1, '01Sep1955', '2400', '24Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Sep1955', '2400', '24Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

!   There will be 1 more than the 1000 values...  For now, ignore the last value (1001)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(1001, nvals, 'test_ITS_CharNotes Basic number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(14026,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_ITS_CharNotes Basic precision stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes1, itimes2, baseDate, granularity, 1000, 'test_ITS_CharNotes Basic Quality, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, 1000, 'test_ITS_CharNotes Basic Values, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, 1000, 'test_ITS_CharNotes Basic Quality, loc 50', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, 1000, 'test_ITS_CharNotes cNotes, loc 50', status)
    if (status.ne.0) go to 900

    lenQuality = 2
    lenNotes = 4
!
      
    nvals = 1000
      call datjul('16Sep1955', juls, status)
      juls = juls * 1440
      ich = ichar('A')
      do i=1, nvals
       itimes1(i) = (i * 1440) + juls
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i) 
       data1(i) =FLOAT(i)
       data3(i) = data1(i)
       cnotes1(i) = ' '
       n = mod(i-1,25) + 1
       if (n.eq.1) lowerCase = .not.lowerCase
       if (lowerCase) then
       do k=1,n
          cnotes1(i)(k:k) = char(ich+k-1+32)          
       end do
       else
       do k=1,n
          cnotes1(i)(k:k) = char(ich+k-1)          
       end do
       endif       
       do j=1,2
        quality1(j,i) = i+j
        quality3(j,i) = quality1(j,i)
       end do
      
       end do
    kvals = nvals   
! 
    cpath1 = '/Time Series With Notes/Basic 2/Flow/date/IR-year//'
    
    nvals = 365
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    zero = 0
    kvals = 1000
    call ztsIrregRetClear(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(365, nvals, 'test_ITS_CharNotes Basic number stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(5035,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 100', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotes Basic precision stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 100', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 100', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 100', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 100', status)
    if (status.ne.0) go to 900

  
    !  Store 3 new notes 
    nvals = 3
    cnotes1(25) = 'this is the first of three lines'
    cnotes1(26) = 'A second line'
    cnotes1(27) = 'The third line for these notes'
    cnotes3(25) = cnotes1(25)
    cnotes3(26) = cnotes1(26)
    cnotes3(27) = cnotes1(27)
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1(25), nvals, &
    data1(25), 2, quality1(1,25), 2, notes, 0, cnotes1(25), 5, userHeader, 0, &  
    'CFS', 'PER-AVER', 3, '', 0, status)
    
    if (status.ne.0) go to 900
    timeGranularitySeconds = 60
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    zero = 0
    kvals = 1000
    call ztsIrregRetClear(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(365, nvals, 'test_ITS_CharNotes Basic number stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(5082,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 110', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 110', status)
    if (status.ne.0) go to 900
    !Not a vaild check here
    !call checkNumbers(3, precision, 'test_ITS_CharNotes Basic precision stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 110', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 110', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 110', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 110', status)
    if (status.ne.0) go to 900
    
    !  Now don't store notes one day later (24 vals)
    nvals = 3
    
    cnotes1(25) = ''
    cnotes1(26) = ''
    cnotes1(27) = ''
    cnotes3(25) = cnotes1(25)
    cnotes3(26) = cnotes1(26)
    cnotes3(27) = cnotes1(27)
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1(25), nvals, &
    data1(25), 2, quality1(1,25), 2, notes, 0, cnotes1(25), 5, userHeader, 0, &  
    'CFS', 'PER-AVER', 4, '', 0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    zero = 0
    kvals = 1000
    call ztsIrregRetClear(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(365, nvals, 'test_ITS_CharNotes Basic number stored, loc 120', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 120', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 120', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 120', status)
    if (status.ne.0) go to 900
    call checkNumbers(5007,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 120', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 120', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 120', status)
    if (status.ne.0) go to 900
    !Not a vaild check here
    !call checkNumbers(4, precision, 'test_ITS_CharNotes Basic precision stored, loc 120', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 120', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 120', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 120', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 120', status)
    if (status.ne.0) go to 900
    
    cnotes3(25) = cnotes1(25)
    cnotes3(26) = cnotes1(26)
    cnotes3(27) = cnotes1(27)
    
    !  Now write the data set with notes, and then without notes
    !  Under this case, the notes should be deleted on the second write
    nvals = 100
    cpath1 = '/Time Series With Notes/Basic 3/Flow/date/IR-year//'
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 1,  '',  0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    IOFSET = 0
    KVALS = nvals
    max = 4
    zero = 0

    !  check same exact data
        zero = 0
    kvals = 1000
    call ztsIrregRetClear(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(100, nvals, 'test_ITS_CharNotes Basic number stored, loc 130', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 130', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 130', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 130', status)
    if (status.ne.0) go to 900
    call checkNumbers(1372,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 130', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 130', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 130', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_ITS_CharNotes Basic precision stored, loc 130', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 130', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 130', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 130', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 130', status)
    if (status.ne.0) go to 900
    

   
    
    !  Now write without notes
    nvals = 100
    cpath1 = '/Time Series With Notes/Basic 3/Flow/date/IR-year//'
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 0, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    !  And check
        zero = 0
    kvals = 1000
    call ztsIrregRetClear(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*kvals), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)
    
    do i=1,nvals
       cnotes3(i) = ''
    end do

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(100, nvals, 'test_ITS_CharNotes Basic number stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(100,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 140', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotes Basic precision stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 140', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 140', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 140', status)
    if (status.ne.0) go to 900
   !  call checkNumbers(0, totalNotesRead, 'test_ITS_CharNotes Basic number stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 140', status)
    if (status.ne.0) go to 900
    
     do i=1,nvals
       cnotes3(i) = cnotes1(i)
     end do
  
    
    !  Now store the full array
    nvals = 1000
    cpath1 = '/Time Series With Notes/Basic 4/Flow/date/IR-year//'
    timeGranularitySeconds = 60
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 1,  '',  0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    zero = 0
    kvals = 1000
    call ztsIrregRetClear(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '06Sep1955', '2400', '04Dec1958', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals, nvals, 'test_ITS_CharNotes Basic number stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotes lenValuesRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotes lenQualityRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotes lenNotesRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(13972,totalNotesRead, 'test_ITS_CharNotes totalNotesRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotes Basic units, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotes Basic type, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_ITS_CharNotes Basic precision stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotes Basic Quality, loc 150', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotes Basic Values, loc 150', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_ITS_CharNotes Basic Quality, loc 150', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotes cNotes, loc 150', status)
    if (status.ne.0) go to 900
   
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Basic Irregular Time Series with cnotes Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Basic Irregular Time Series with cnotes ') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end