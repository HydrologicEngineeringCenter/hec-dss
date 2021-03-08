    SUBROUTINE test_RTS_Repeats(ifltab1, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
    !  Write to DSS array 1, read into array 2, compare with array 3
      !double precision data1(1010), data2(1010), data3(1010)
      double precision data1(1010), data2(1010), data3(1010)
      integer quality1(2,1010), notes1(4,1010)
      integer quality2(2,1010), notes2(4,1010)
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12, cnotes*1
      integer precision
      integer nvals, kvals, i, iofset, max, zero
      integer lenQuality, lenNotes 
      integer length
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer maxVals
!
      integer userHeader(1)
      double precision coordinates(1)
      integer icoord(1)  
      
      common /lchk/ lcheck
      logical lcheck 
      
      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_RTS_Repeats'   
      lenQuality = 2
      lenNotes = 4
      maxVals = 1010
    
!
      nvals = 100
      do 20 i=1, nvals
       data1(i) = 123.0    
 20   continue
     data1(50) = 50.0
     data1(51) = 51.0

     do 40 i=1, nvals 
       data3(i) = data1(i)    
 40   continue
! 
    cpath1 = '/Time Series/Repeats/Flow/date/1Hour//'
    
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes, 0, userHeader, 0, & 
    'CFS', 'PER-AVER', 2, '',  0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    IOFSET = 0
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0

    !  First - check same exact data
    call ztsRegRetClear(ifltab1, cpath1, '08JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '10JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_Repeats number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_Repeats lenQualityRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_Repeats lenNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_Repeats number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Repeats units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Repeats type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_Repeats precision, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_Repeats Values, loc 10', status)
    if (status.ne.0) go to 900

    do 120, i=1,nvals
        data3(i) = zmissingFlag()
120 continue

    !  Now, before...  all data should be missing
    call ztsRegRetClear(ifltab1, cpath1, '08JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '02JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_Repeats number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_Repeats lenQualityRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_Repeats lenNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_Repeats number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_Repeats number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Repeats units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Repeats type, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_Repeats precision, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_Repeats Values, loc 20', status)
    if (status.ne.0) go to 900


 !  Now, after...  all data should be missing
    call ztsRegRetClear(ifltab1, cpath1, '08JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '20JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_Repeats number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_Repeats number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_Repeats lenQualityRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_Repeats lenNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_Repeats number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Repeats units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Repeats type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_Repeats precision, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_Repeats Values, loc 30', status)
    if (status.ne.0) go to 900



 !  Now, partly before, so 1/2 of data is there, the other missing
    do 140 i=49,100
        data3(i) = 123.0
140 continue
     data3(98) = 50.0
     data3(99) = 51.0
     call ztsRegRetClear(ifltab1, cpath1, '08JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_Repeats number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_Repeats number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_Repeats lenQualityRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_Repeats lenNotesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_Repeats number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Repeats units, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Repeats type, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_Repeats precision, loc 40', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_Repeats Values, loc 40', status)
    if (status.ne.0) go to 900
   

 !  Now, partly after, so 1/2 of data is there, the other missing
    nvals = 110
    kvals = 110
    do 150 i=1,52
        data3(i) = 123.0
150 continue
    do 160 i=53,nvals
         data3(i) = zmissingFlag()
160 continue
     data3(2) = 50.0
     data3(3) = 51.0
     zero = 0
     call ztsRegRetClear(ifltab1, cpath1, '08JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_Repeats number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_Repeats number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_Repeats lenQualityRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_Repeats lenNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_Repeats number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Repeats units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Repeats type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_Repeats precision, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_Repeats Values, loc 50', status)
    if (status.ne.0) go to 900

 !  Now, in the middle, so we are only reading valid data
    nvals = 50
    kvals = nvals
    do 200 i=1,nvals
        data3(i) = 123.0
200 continue
     data3(2) = 50.0
     data3(3) = 51.0
     call ztsRegRetClear(ifltab1, cpath1, '08JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_Repeats number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_Repeats number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_Repeats lenQualityRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_Repeats lenNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_Repeats number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Repeats units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Repeats type, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_Repeats precision, loc 60', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_Repeats Values, loc 60', status)
    if (status.ne.0) go to 900


 !  Now, before and after, so there is missing data on both sides
    nvals = 200
    kvals = 200
    do 250 i=1,nvals
        data3(i) = zmissingFlag()
250 continue
    do 260 i=49,148
         data3(i) = 123.0
260 continue
     data3(98) = 50.0
     data3(99) = 51.0
     
    call ztsRegRetClear(ifltab1, cpath1, '08JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_Repeats number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_Repeats number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_Repeats lenQualityRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_Repeats lenNotesRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_Repeats number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Repeats units, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Repeats type, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_Repeats precision, loc 70', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_Repeats Values, loc 70', status)
    if (status.ne.0) go to 900
   
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Full Regular Time Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Full Regular Time Series') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 1      Format('Double Regular Time Series Full test FAILED')
 910    Continue
        if (status.eq.0) status = -1
        return
        end