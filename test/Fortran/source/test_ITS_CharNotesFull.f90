    SUBROUTINE test_ITS_CharNotesFull(ifltab1, messageUnit, status)
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
      integer itimes1(2200), itimes2(2200), itimes3(2200), itimes4(2200)
      integer jtimes1(1), jtimes2(1)
      double precision xdata1(1), xdata2(1)
    !  Write to DSS array 1, read into array 2, compare with array 3
      !double precision data1(1010), data2(1010), data3(1010)
      double precision data1(2200), data2(2200), data3(2200), data4(2200)   
      integer quality1(2,2200)
      integer quality2(2,2200), notes2(4,2200)
      integer quality3(2,2200)
      character cnotes1(2200)*50
      character cnotes2(2200)*50
      character cnotes3(2200)*50
      character cnotes4(2200)*50
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12
      integer notes(1)
      integer nvals, i
      integer JULS
      integer baseDate
      integer j, readFlag
      integer numberVals, lengthEachValue, lengthEachQuality, lengthEachNote
      integer totalLengthCNotes
      integer kvals, zero
      integer lenQuality
      integer length
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer granularity, precision
      integer lengthUserHeader
      integer timeGranularitySeconds
      
      integer userHeader(1)
      double precision coordinates(3)
      integer icoord(6)
      integer k,n,ich
      logical lowerCase
      
      common /lchk/ lcheck
      logical lcheck
!
!
      WRITE(messageUnit, *)'Begin test_ITS_CharNotesFull'
      baseDate = 0
      nvals = 1000
      call datjul("06Sep1955", juls, status)
      juls = juls * 1440      
      ich = ichar('A')
      lowerCase = .false.
      do i=1, nvals
       itimes1(i) = (i * 1440) + juls
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i) 
       data1(i) = i
       data3(i) = data1(i)
       data4(i) = data1(i)
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
       cnotes3(i) = cnotes1(i)
       cnotes4(i) = cnotes1(i)     
       do j=1,2
        quality1(j,i) = i+j
        quality3(j,i) = quality1(j,i)
       end do
      end do
 
 
!     call zset('mlvl', ' ', 12)
!
!   First, write a single value and then read it back
 1  Format('Full Char Notes Irregular test FAILED')   
    goto 1234

    !  write 1000 values and check
    cpath1 = '/Full Char Notes Irregular/several/Flow/date/~1Day/F/'
    nvals = 1000
    
    timeGranularitySeconds = INTVL_1_MINUTE
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 1,  '',  0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    

    kvals = 2200
    lenQuality = 2
    readFlag = 0
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
    call checkNumbers(1000, nvals, 'test_ITS_CharNotesFull number stored, loc 10', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(14000,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_ITS_CharNotesFull precision stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotesFull Values, loc 10', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 10', status)
    if (status.ne.0) go to 900 

    !  Test getNumberValues function
    call ztsGetSizes(ifltab1, cpath1, '25DEC1955', '1400', '04JAN1958', '0800', &
    numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    !  The above call is for ALL values in the records within time range.
    call checkNumbers(1000, numberVals, 'test_ITS_CharNotesFull ztsGetSizes number stored, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachValue, 'test_ITS_CharNotesFull ztsGetSizes lengthEachValue, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachQuality, 'test_ITS_CharNotesFull ztsGetSizes lengthEachQuality, loc 11', status)
    if (status.ne.0) go to 900
    call checkNumbers(0, lengthEachNote, 'test_ITS_CharNotesFull ztsGetSizes lengthEachNote, loc 11', status)
    if (status.ne.0) go to 900
  
    
    !Re-write in place
    cpath1 = '/Full Char Notes Irregular/several/Flow/date/~1Day/F/'
    nvals = 1000
    baseDate = 0
    timeGranularitySeconds = INTVL_1_MINUTE
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    

    NVALS = 1010
    baseDate = 0
    NVALS = 1010
    kvals = 2200

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
    call checkNumbers(1000, nvals, 'test_ITS_CharNotesFull number stored, loc 15', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 15', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 15', status)
    if (status.ne.0) go to 900
    call checkNumbers(14000,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 15', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 15', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 15', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 15', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 15', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotesFull Values, loc 15', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 15', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 15', status)
    if (status.ne.0) go to 900 
    
    	
    !  Read a single value
    baseDate = 0
    NVALS = 1010
    readFlag = 0
    
    call ztsIrregRetClear(ifltab1, cpath1, '01Jan1956', '2400', '01Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, 3, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Jan1956', '2400', '01Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(1, nvals, 'test_ITS_CharNotesFull nvals stored, loc 17', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 17', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 17', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 17', status)
    if (status.ne.0) go to 900
    call checkNumbers(19,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 17', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3(118), itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 17', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 17', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 17', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 17', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(118), data2, nvals, 'test_ITS_CharNotesFull Values, loc 17', status)
    if (status.ne.0) go to 900
    call checkInts(quality3(1,118), quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 17', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(118), cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 17', status)
    if (status.ne.0) go to 900
    
  !  Read data getting previous value in previous block
    baseDate = 0
    NVALS = 1010
    readFlag = 1
    
    call ztsIrregRetClear(ifltab1, cpath1, '01Jan1956', '2400', '01Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Jan1956', '2400', '01Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(2, nvals, 'test_ITS_CharNotesFull nvals stored, loc 18', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 18', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 18', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 18', status)
    if (status.ne.0) go to 900
    call checkNumbers(37,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 18', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3(117), itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 18', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 18', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 18', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 18', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(117), data2, nvals, 'test_ITS_CharNotesFull Values, loc 18', status)
    if (status.ne.0) go to 900
    call checkInts(quality3(1,117), quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 18', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(117), cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 18', status)
    if (status.ne.0) go to 900
    
    	    	
    !  Read data getting previous value in same block
    baseDate = 0
    NVALS = 1010
    readFlag = 1
    
    call ztsIrregRetClear(ifltab1, cpath1, '02Jan1956', '2400', '02Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '02Jan1956', '2400', '02Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(2, nvals, 'test_ITS_CharNotesFull nvals stored, loc 19', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 19', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 19', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 19', status)
    if (status.ne.0) go to 900
    call checkNumbers(39,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 19', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3(118), itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 19', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 19', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 19', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 19', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(118), data2, nvals, 'test_ITS_CharNotesFull Values, loc 19', status)
    if (status.ne.0) go to 900
    call checkInts(quality3(1,118), quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 19', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(118), cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 19', status)
    if (status.ne.0) go to 900
    
    
    !   Now subsequent value in same block
    readFlag = 2
    call ztsIrregRetClear(ifltab1, cpath1, '01Jan1956', '2400', '01Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Jan1956', '2400', '01Jan1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(2, nvals, 'test_ITS_CharNotesFull nvals stored, loc 20', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(39,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3(118), itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(118), data2, nvals, 'test_ITS_CharNotesFull Values, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(quality3(1,118), quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 20', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(118), cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 20', status)
    if (status.ne.0) go to 900
    

!   Now subsequent value in same block
!   Last value should be for 31 Dec 1955, 2400
    readFlag = 2
    call ztsIrregRetClear(ifltab1, cpath1, '26Dec1955', '2400', '30Dec1955', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '26Dec1955', '2400', '30Dec1955', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(6, nvals, 'test_ITS_CharNotesFull nvals stored, loc 24', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 24', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 24', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 24', status)
    if (status.ne.0) go to 900
    call checkNumbers(93,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 24', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3(112), itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 24', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 24', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 24', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 24', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(112), data2, nvals, 'test_ITS_CharNotesFull Values, loc 24', status)
    if (status.ne.0) go to 900
    call checkInts(quality3(1,112), quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 24', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(112), cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 24', status)
    if (status.ne.0) go to 900

    !   Subsequent value in next block
    NVALS = 1010
    !   last value should be for 01 Jan 1956, 2400 
    readFlag = 2
    call ztsIrregRetClear(ifltab1, cpath1, '26Dec1955', '2400', '31Dec1955', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '26Dec1955', '2400', '31Dec1955', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(7, nvals, 'test_ITS_CharNotesFull nvals stored, loc 25', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 25', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 25', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 25', status)
    if (status.ne.0) go to 900
    call checkNumbers(112,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 25', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3(112), itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 25', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 25', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 25', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 25', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(112), data2, nvals, 'test_ITS_CharNotesFull Values, loc 25', status)
    if (status.ne.0) go to 900
    call checkInts(quality3(1,112), quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 25', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(112), cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 25', status)
    if (status.ne.0) go to 900

 !   Now previous and subsequent value in differnt blocks
    baseDate = 0
    NVALS = 1010   
    readFlag = 3
     call ztsIrregRetClear(ifltab1, cpath1, '01Jan1956', '2400', '31Dec1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Jan1956', '2400', '31Dec1956', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(368, nvals, 'test_ITS_CharNotesFull nvals stored, loc 26', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 26', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 26', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 26', status)
    if (status.ne.0) go to 900
    call checkNumbers(5152,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 26', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3(117), itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 26', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 26', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 26', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_ITS_CharNotesFull precision stored, loc 26', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3(117), data2, nvals, 'test_ITS_CharNotesFull Values, loc 26', status)
    if (status.ne.0) go to 900
    call checkInts(quality3(1,117), quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Basic Quality, loc 26', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(117), cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 26', status)
    if (status.ne.0) go to 900

 1234  continue   
     !  write 1000 values and check
    cpath1 = '/Full Char Notes Irregular/Replaced/Flow/date/~1Day/F/'
    nvals = 1000
    timeGranularitySeconds = INTVL_1_MINUTE
    
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 3, '', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    

 
    NVALS = 1010
    kvals = 2200
    lenQuality = 2
    readFlag = 0
    zero = 0

    call ztsIrregRetClear(ifltab1, cpath1, '01Sep1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Sep1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(1000, nvals, 'test_ITS_CharNotesFull number stored, loc 27', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 27', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 27', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 27', status)
    if (status.ne.0) go to 900
    call checkNumbers(14000,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 27', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 27', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 27', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 27', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_ITS_CharNotesFull precision stored, loc 27', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotesFull Values, loc 27', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Quality, loc 27', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 27', status)
    if (status.ne.0) go to 900 

    !offset by 12 hours and replace full data set
    nvals = 1000
    do i=1,nvals
        itimes1(i) = itimes1(i) - 720
        data1(i) = data1(i) - 0.5
    end do
    
    baseDate = 0    
    timeGranularitySeconds = INTVL_1_MINUTE
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 4, '', 1, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900


    kvals = 2200   
    readFlag = 0
     call ztsIrregRetClear(ifltab1, cpath1, '01Sep1955', '2400', '31Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Sep1955', '2400', '31Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(1001, nvals, 'test_ITS_CharNotesFull nvals stored, loc 28', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 28', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 28', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 28', status)
    if (status.ne.0) go to 900
    call checkNumbers(14026,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 28', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 28', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 28', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_ITS_CharNotesFull precision stored, loc 28', status)
    if (status.ne.0) go to 900
    
    call checkTimes(itimes1, itimes2, baseDate, granularity, (nvals-1), 'test_ITS_CharNotesFull times, loc 28', status)
    if (status.ne.0) go to 900
    call checkDoubles(data1, data2, (nvals-1), 'test_ITS_CharNotesFull Values, loc 28', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQualityRead, (nvals-1), 'test_ITS_CharNotesFull Quality, loc 28', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, (nvals-1), 'test_ITS_CharNotesFull cNotes, loc 28', status)
    if (status.ne.0) go to 900
    
    !
   
     !  write 1000 values and check
    cpath1 = '/Full Char Notes Irregular/Merged/Flow/date/~1Day/F/'
    nvals = 1000
    do i=1,nvals
        itimes1(i) = itimes4(i)
        data1(i) = data4(i)
    end do
    
    timeGranularitySeconds = INTVL_1_MINUTE
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 0, '',  0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    

 
    NVALS = 1010
    kvals = 2200
    lenQuality = 2
    readFlag = 0

    call ztsIrregRetClear(ifltab1, cpath1, '01Sep1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Sep1955', '2400', '30Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(1000, nvals, 'test_ITS_CharNotesFull number stored, loc 29', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 29', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 29', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 29', status)
    if (status.ne.0) go to 900
    call checkNumbers(14000,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 29', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, granularity, nvals, 'test_ITS_CharNotesFull times, loc 29', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 29', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 29', status)
    if (status.ne.0) go to 900
    call checkNumbers(0, precision, 'test_ITS_CharNotesFull precision stored, loc 29', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_ITS_CharNotesFull Values, loc 29', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQualityRead, nvals, 'test_ITS_CharNotesFull Quality, loc 29', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_ITS_CharNotesFull cNotes, loc 29', status)
    if (status.ne.0) go to 900 

    !offset by 12 hours and merge full data set
    nvals = 1000
    do i=1,nvals
        itimes1(i) = itimes1(i) - 720
        itimes3(i) = itimes1(i)
        data1(i) = data1(i) - 0.5
    end do
    
    baseDate = 0    
    timeGranularitySeconds = INTVL_1_MINUTE
    call ztsIrregStoreFull (ifltab1, cpath1, '', timeGranularitySeconds, itimes1, nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 1,  '',  0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900


    kvals = 2200   
    readFlag = 0
     call ztsIrregRetClear(ifltab1, cpath1, '01Sep1955', '2400', '31Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

    call ztsIrregRetrieveFull(ifltab1, cpath1, '01Sep1955', '2400', '31Dec1959', '2400', kvals, nvals, &
    itimes2, granularity, baseDate, data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*2100), totalNotesRead, userHeader, zero, length, &
     cunits, ctype, precision, ctemp, readFlag, status)
     

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900    
    call checkNumbers(2000, nvals, 'test_ITS_CharNotesFull nvals stored, loc 30', status)
    if (status.ne.0) go to 900    
    call checkNumbers(2,lenValuesRead, 'test_ITS_CharNotesFull lenValuesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_ITS_CharNotesFull lenQualityRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_ITS_CharNotesFull lenNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(28000,totalNotesRead, 'test_ITS_CharNotesFull totalNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_CharNotesFull units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_CharNotesFull type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_ITS_CharNotesFull precision stored, loc 30', status)
    if (status.ne.0) go to 900
    !
    !  Since data merged, we need to check times and values in a loop
    jtimes1(1) = itimes1(1)
    xdata1(1) = 0.5
    do i=1,nvals
        !  This is to avoid a compilier issue
        jtimes2(1) = itimes2(i)
        call checkTimes(jtimes1, jtimes2, baseDate, granularity, 1, 'test_ITS_CharNotesFull times, loc 30', status)
        if (status.ne.0) go to 900
        xdata2(1) = data2(i)
        call checkDoubles(xdata1, xdata2, 1, 'test_ITS_CharNotesFull Values, loc 30', status)
        if (status.ne.0) go to 900
        xdata1(1) = xdata1(1) + 0.5
        jtimes1(1) = jtimes1(1) + 720
    end do


 500  continue   
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