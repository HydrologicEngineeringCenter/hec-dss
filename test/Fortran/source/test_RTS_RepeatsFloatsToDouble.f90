    SUBROUTINE test_RTS_RepeatsFloatsToDouble(ifltab1, messageUnit, status)
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
      real data1(1010)
      double precision data2(1010), data3(1010)
      integer quality1(1,1010), notes1(3,1010)
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
      double precision coordinates(1)
      integer icoord(1)  

      common /lchk/ lcheck
      logical lcheck       
      
      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_RTS_RepeatsFloatsToDouble'   
      maxVals = 1010
      lenQuality = 2
      lenNotes = 4
    
!
      nvals = 100
      j = 1
      do i=1, nvals
       data1(i) = FLOAT(i)
       data3(i) = data1(i) 
        quality1(1,i) = (i*100) + j
        if ((i.gt.10).and.(i.lt.30)) quality1(1,i) = 1234
        quality3(1,i) = quality1(1,i) 
        quality3(2,i) = 0 
        notes3(4,i) = 0
        do j=1,3
          notes1(j,i) = (i*10000) + j
          if ((i.gt.10).and.(i.lt.30)) notes1(j,i) = 4567
          notes3(j,i) = notes1(j,i)
        end do
      end do

! 
    cpath1 = '/Time Series/Repeats/Quality and Notes/date/1Hour/Floats to Doubles/'
    
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 1, quality1, 1, notes1, 3, cnotes, 0, userHeader, 0, &  
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
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '10JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_RepeatsFloatsToDouble number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenValuesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenQualityRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(3,lenNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_RepeatsFloatsToDouble units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_RepeatsFloatsToDouble type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_RepeatsFloatsToDouble precision, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_RepeatsFloatsToDouble Values, loc 10', status)
    if (status.ne.0) go to 900 
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_RepeatsFloatsToDouble Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_RTS_RepeatsFloatsToDouble Notes, loc 10', status)
    if (status.ne.0) go to 900

    do i=1,nvals
        data3(i) = zmissingFlag()        
        quality3(1,i) = 0 
        quality3(2,i) = 0
        do j=1,4
            notes3(j,i) = 0
        end do
    end do

    !  Now, before...  all data should be missing
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '02JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_RepeatsFloatsToDouble number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenValuesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenQualityRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(3,lenNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_RepeatsFloatsToDouble units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_RepeatsFloatsToDouble type, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_RepeatsFloatsToDouble precision, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_RepeatsFloatsToDouble Values, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_RepeatsFloatsToDouble Quality, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_RTS_RepeatsFloatsToDouble Notes, loc 20', status)
    if (status.ne.0) go to 900


     !  Now, after...  all data should be missing
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '20JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_RepeatsFloatsToDouble number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenValuesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenQualityRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(3,lenNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_RepeatsFloatsToDouble units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_RepeatsFloatsToDouble type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_RepeatsFloatsToDouble precision, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_RepeatsFloatsToDouble Values, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_RepeatsFloatsToDouble Quality, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_RTS_RepeatsFloatsToDouble Notes, loc 30', status)
    if (status.ne.0) go to 900



 !  Now, partly before, so 1/2 of data is there, the other missing
    do i=49,100
        data3(i) = data1(i - 48)
        quality3(1,i) = quality1(1,i-48)
        quality3(2,i) = 0 
        notes3(4,i) = 0
        do j=1,3
            notes3(j,i) = notes1(j,i-48)
        end do
    end do

    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_RepeatsFloatsToDouble number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenValuesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenQualityRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(3,lenNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_RepeatsFloatsToDouble units, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_RepeatsFloatsToDouble type, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_RepeatsFloatsToDouble precision, loc 40', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_RepeatsFloatsToDouble Values, loc 40', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_RepeatsFloatsToDouble Quality, loc 40', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_RTS_RepeatsFloatsToDouble Notes, loc 40', status)
    if (status.ne.0) go to 900
   

 !  Now, partly after, so 1/2 of data is there, the other missing
    nvals = 110
    kvals = 110
    do i=1,52
        data3(i) = data1(i + 48)        
        quality3(1,i) = quality1(1,i+48)
        quality3(2,i) = 0 
        notes3(4,i) = 0
        do j=1,3
            notes3(j,i) = notes1(j,i+48)
        end do
    end do

    do i=53,nvals
         data3(i) = zmissingFlag()         
         quality3(1,i) = 0
         quality3(2,i) = 0  
        do j=1,4
            notes3(j,i) = 0
        end do
    end do

    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_RepeatsFloatsToDouble number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenValuesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenQualityRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(3,lenNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_RepeatsFloatsToDouble units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_RepeatsFloatsToDouble type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_RepeatsFloatsToDouble precision, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_RepeatsFloatsToDouble Values, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_RepeatsFloatsToDouble Quality, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_RTS_RepeatsFloatsToDouble Notes, loc 50', status)
    if (status.ne.0) go to 900

 !  Now, in the middle, so we are only reading valid data
    nvals = 50
    kvals = nvals
    do i=1,nvals
        data3(i) = data1(i + 48)
    end do

    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_RepeatsFloatsToDouble number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenValuesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenQualityRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(3,lenNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_RepeatsFloatsToDouble units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_RepeatsFloatsToDouble type, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_RepeatsFloatsToDouble precision, loc 60', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_RepeatsFloatsToDouble Values, loc 60', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_RepeatsFloatsToDouble Quality, loc 60', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_RTS_RepeatsFloatsToDouble Notes, loc 60', status)
    if (status.ne.0) go to 900

 !  Now, before and after, so there is missing data on both sides
    nvals = 200
    kvals = 200
    do i=1,nvals
        data3(i) = zmissingFlag()       
        quality3(1,i) = 0
        quality3(2,i) = 0
        do j=1,4
            notes3(j,i) = 0
        end do
    end do
    do i=49,148
         data3(i) = data1(i-48)
         quality3(1,i) = quality1(1,i-48)
         quality3(2,i) = 0
         notes3(4,i) = 0
        do j=1,3
            notes3(j,i) = notes1(j,i-48)
        end do
    end do

    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, 2, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_RepeatsFloatsToDouble number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenValuesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(1,lenQualityRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(3,lenNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_RepeatsFloatsToDouble number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_RepeatsFloatsToDouble units, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_RepeatsFloatsToDouble type, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, precision, 'test_RTS_RepeatsFloatsToDouble precision, loc 70', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_RepeatsFloatsToDouble Values, loc 70', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_RepeatsFloatsToDouble Quality, loc 70', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, 4, nvals, 'test_RTS_RepeatsFloatsToDouble Notes, loc 70', status)
    if (status.ne.0) go to 900
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('test_RTS_RepeatsFloatsToDouble Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in test_RTS_RepeatsFloatsToDouble') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 1      Format('test_RTS_RepeatsFloatsToDouble FAILED')
 910    Continue
        if (status.eq.0) status = -1
        return
        end
