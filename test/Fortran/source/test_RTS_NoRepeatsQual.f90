    SUBROUTINE test_RTS_NoRepeatsQual(ifltab1, messageUnit, status)
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
      integer quality3(2,1010), notes3(4,1010)
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12, cnotes*1
      integer precision
      integer nvals, kvals, i, j, iofset, max, zero
      integer lenQuality, lenNotes 
      integer length
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer maxVals
      
!
      integer userHeader(1)

      common /lchk/ lcheck
      logical lcheck
      
      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_RTS_NoRepeatsQual'   
      maxVals = 1010
    lenQuality = 2
    lenNotes = 4
!
      nvals = 100
      do 120 i=1, nvals
       data1(i) = FLOAT(i)
       data3(i) = data1(i) 
       do 110 j=1,2
        quality1(j,i) = (i*100) + j
        quality3(j,i) = quality1(j,i) 
 110    continue 
        do 120 j=1,4
        notes1(j,i) = (i*10000) + j
        notes3(j,i) = notes1(j,i)        
 120  continue
! 
    cpath1 = '/Time Series/No Repeats/Quality and Notes/date/1Hour//'
    
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 2, notes1, 4, cnotes, 0, userHeader, 0, &  
    'CFS', 'PER-AVER', 4, '',  0, status)
    

    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    IOFSET = 0
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0

    !  First - check same exact data
    call ztsRetrieveRegArgs(ifltab1, cpath1, '10JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)
    

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_NoRepeatsQual number stored, loc 10', status)
    if (status.ne.0) go to 900
     call checkNumbers(2,lenValuesRead, 'test_RTS_NoRepeatsQual lenValuesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_NoRepeatsQual lenQualityRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_NoRepeatsQual lenNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_NoRepeatsQual totalNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_NoRepeatsQual units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_NoRepeatsQual type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_NoRepeatsQual precision, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_NoRepeatsQual Values, loc 10', status)
    if (status.ne.0) go to 900 
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_NoRepeatsQual Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_NoRepeatsQual Notes, loc 10', status)
    if (status.ne.0) go to 900

    do 170, i=1,nvals
        data3(i) = zmissingFlag()
        do 165 j=1,2
            quality3(j,i) = 0 
 165    continue 
        do 170 j=1,4
            notes3(j,i) = 0
170 continue

    !  Now, before...  all data should be missing
    call ztsRetrieveRegArgs(ifltab1, cpath1, '02JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_NoRepeatsQual number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_NoRepeatsQual lenValuesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_NoRepeatsQual lenQualityRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_NoRepeatsQual units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_NoRepeatsQual type, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_NoRepeatsQual precision, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_NoRepeatsQual Values, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_NoRepeatsQual Quality, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_NoRepeatsQual Notes, loc 20', status)
    if (status.ne.0) go to 900


 !  Now, after...  all data should be missing
    call ztsRetrieveRegArgs(ifltab1, cpath1, '20JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_NoRepeatsQual number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_NoRepeatsQual lenValuesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_NoRepeatsQual lenQualityRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_NoRepeatsQual units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_NoRepeatsQual type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_NoRepeatsQual precision, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_NoRepeatsQual Values, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_NoRepeatsQual Quality, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_NoRepeatsQual Notes, loc 30', status)
    if (status.ne.0) go to 900



 !  Now, partly before, so 1/2 of data is there, the other missing
    do 240 i=49,100
        data3(i) = data1(i - 48)
        do 265 j=1,2
            quality3(j,i) = quality1(j,i-48) 
265    continue 
        do 240 j=1,4
            notes3(j,i) = notes1(j,i-48)
240 continue


    max = 4
    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_NoRepeatsQual number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_NoRepeatsQual lenValuesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_NoRepeatsQual lenQualityRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_NoRepeatsQual units, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_NoRepeatsQual type, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_NoRepeatsQual precision, loc 40', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_NoRepeatsQual Values, loc 40', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_NoRepeatsQual Quality, loc 40', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_NoRepeatsQual Notes, loc 40', status)
    if (status.ne.0) go to 900
   

 !  Now, partly after, so 1/2 of data is there, the other missing
    nvals = 110
    kvals = 110
    do 350 i=1,52
        data3(i) = data1(i + 48)
           do 365 j=1,2
            quality3(j,i) = quality1(j,i+48) 
365    continue 
        do 350 j=1,4
            notes3(j,i) = notes1(j,i+48)
350 continue

    do 380 i=53,nvals
         data3(i) = zmissingFlag()
          do 375 j=1,2
            quality3(j,i) = 0 
 375    continue 
        do 380 j=1,4
            notes3(j,i) = 0
380 continue

     zero = 0
     max = 4
    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_NoRepeatsQual number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_NoRepeatsQual lenValuesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_NoRepeatsQual lenQualityRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_NoRepeatsQual units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_NoRepeatsQual type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_NoRepeatsQual precision, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_NoRepeatsQual Values, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_NoRepeatsQual Quality, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_NoRepeatsQual Notes, loc 50', status)
    if (status.ne.0) go to 900

 !  Now, in the middle, so we are only reading valid data
    nvals = 50
    kvals = nvals
    do 200 i=1,nvals
        data3(i) = data1(i + 48)
200 continue

    max = 4
    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_NoRepeatsQual number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_NoRepeatsQual lenValuesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_NoRepeatsQual lenQualityRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_NoRepeatsQual units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_NoRepeatsQual type, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_NoRepeatsQual precision, loc 60', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_NoRepeatsQual Values, loc 60', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_NoRepeatsQual Quality, loc 60', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_NoRepeatsQual Notes, loc 60', status)
    if (status.ne.0) go to 900

 !  Now, before and after, so there is missing data on both sides
    nvals = 200
    kvals = 200
    do 450 i=1,nvals
        data3(i) = zmissingFlag()
         do 475 j=1,2
            quality3(j,i) = 0 
 475    continue 
        do 450 j=1,4
            notes3(j,i) = 0
450 continue
    do 490 i=49,148
         data3(i) = data1(i-48)
          do 485 j=1,2
            quality3(j,i) = quality1(j,i-48) 
 485    continue 
        do 490 j=1,4
            notes3(j,i) = notes1(j,i-48)
490 continue

    max = 4
    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_NoRepeatsQual number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_NoRepeatsQual lenValuesRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_NoRepeatsQual lenQualityRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_NoRepeatsQual number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_NoRepeatsQual units, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_NoRepeatsQual type, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_NoRepeatsQual precision, loc 70', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_NoRepeatsQual Values, loc 70', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_NoRepeatsQual Quality, loc 70', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_NoRepeatsQual Notes, loc 70', status)
    if (status.ne.0) go to 900
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('test_RTS_NoRepeatsQual Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in test_RTS_NoRepeatsQual') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 1      Format('test_RTS_NoRepeatsQual FAILED')
 910    Continue
        if (status.eq.0) status = -1
        return
        end