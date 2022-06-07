    SUBROUTINE test_RTS_CharNotesBasic(ifltab1, messageUnit, status)
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
      character cnotes1(1010)*50
      character cnotes2(1010)*50
      character cnotes3(1010)*50
      character blanks(1010)*50
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12
      integer precision
      integer nvals, kvals, i, j, iofset, max, zero, n, k, ich
      integer lenQuality, lenNotes
      integer length
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      
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
      WRITE(messageUnit, *)'Begin test_RTS_CharNotesBasic'   

    lenQuality = 2
    lenNotes = 4
!
      nvals = 100
      ich = ichar('A')
      do i=1, nvals
       data1(i) = FLOAT(i)
       data3(i) = data1(i) 
       cnotes1(i) = ' '
       n = mod(i,25) + 1
       do k=1,n
          cnotes1(i)(k:k) = char(ich+k-1)
       end do
       do j=1,2
        quality1(j,i) = (i*100) + j
        quality3(j,i) = quality1(j,i) 
       end do
        do j=1,4
        notes1(j,i) = (i*10000) + j
        notes3(j,i) = notes1(j,i)        
        end do
      end do
 
     do i=1,1010
       blanks(i) = ' '
     end do
 
     do i=1,nvals
      cnotes3(i) = cnotes1(i)
     end do
! 
    cpath1 = '/Time Series With Notes//Flow/date/1Hour//'
    
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 2, notes1, 0, cnotes1, (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 3, '',  0, status)
    
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
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '10JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesBasic number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesBasic number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotesBasic number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesBasic number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1400,totalNotesRead, 'test_RTS_CharNotesBasic totalNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesBasic units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesBasic type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotesBasic precision, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesBasic Values, loc 10', status)
    if (status.ne.0) go to 900 
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesBasic Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotesBasic cNotes, loc 10', status)
    if (status.ne.0) go to 900
    

    do  i=1,nvals
        data3(i) = zmissingFlag()
        do j=1,2
            quality3(j,i) = 0 
        end do
        do j=1,4
            notes3(j,i) = 0
        end do
    end do

    !  Now, before...  all data should be missing
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '02JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesBasic number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesBasic number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotesBasic number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesBasic number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(100,totalNotesRead, 'test_RTS_CharNotesBasic totalNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesBasic units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesBasic type, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotesBasic precision, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesBasic Values, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, 2, nvals, 'test_RTS_CharNotesBasic Quality, loc 20', status)
    if (status.ne.0) go to 900
    call checkNotes(blanks, cnotes2, nvals, 'test_RTS_CharNotesBasic cNotes, loc 20', status)
    if (status.ne.0) go to 900


 !  Now, after...  all data should be missing
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsRetrieveRegArgs(ifltab1, cpath1, '20JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesBasic nvals stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesBasic lenValuesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotesBasic lenQuality stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesBasic lenNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(100,totalNotesRead, 'test_RTS_CharNotesBasic totalNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesBasic units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesBasic type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotesBasic precision, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesBasic Values, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesBasic Quality, loc 30', status)
    if (status.ne.0) go to 900
    call checkNotes(blanks, cnotes2, nvals, 'test_RTS_CharNotesBasic cNotes, loc 30', status)
    if (status.ne.0) go to 900



 !  Now, partly before, so 1/2 of data is there, the other missing
    do i=1,48
        cnotes3(i) = blanks(1)
    end do
    do i=49,100
        cnotes3(i) = cnotes1(i-48)
        data3(i) = data1(i - 48)
        do j=1,2
            quality3(j,i) = quality1(j,i-48) 
        end do
        do j=1,4
            notes3(j,i) = notes1(j,i-48)
        end do
    end do


    max = 4
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesBasic number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesBasic lenValuesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotesBasic lenQuality stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesBasic lenNotesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(755,totalNotesRead, 'test_RTS_CharNotesBasic totalNotesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesBasic units, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesBasic type, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotesBasic precision, loc 40', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesBasic Values, loc 40', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesBasic Quality, loc 40', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotesBasic cNotes, loc 40', status)
    if (status.ne.0) go to 900
   

 !  Now, partly after, so 1/2 of data is there, the other missing
    nvals = 110
    kvals = 110
    do i=1,52
        cnotes3(i) = cnotes1(i+48)
        data3(i) = data1(i + 48)
           do j=1,2
            quality3(j,i) = quality1(j,i+48) 
           end do
        do j=1,4
            notes3(j,i) = notes1(j,i+48)
        end do
    end do

    do i=53,nvals
        cnotes3(i) = blanks(i)
         data3(i) = zmissingFlag()
          do j=1,2
            quality3(j,i) = 0 
          end do
        do j=1,4
            notes3(j,i) = 0
        end do
    end do

     zero = 0
     max = 4
     call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesBasic nvals stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesBasic lenValuesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotesBasic lenQuality stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesBasic lenNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(786,totalNotesRead, 'test_RTS_CharNotesBasic totalNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesBasic units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesBasic type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotesBasic precision, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesBasic Values, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesBasic Quality, loc 50', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotesBasic cNotes, loc 50', status)
    if (status.ne.0) go to 900

 !  Now, in the middle, so we are only reading valid data
    nvals = 50
    kvals = nvals
    do i=1,nvals
        cnotes3(i) = cnotes1(i+48)
        data3(i) = data1(i + 48)
    end do

    max = 4
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesBasic nvals stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesBasic lenValuesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotesBasic lenQuality stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesBasic lenNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(700,totalNotesRead, 'test_RTS_CharNotesBasic totalNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesBasic units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesBasic type, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotesBasic precision, loc 60', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesBasic Values, loc 60', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesBasic Quality, loc 60', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotesBasic cNotes, loc 60', status)
    if (status.ne.0) go to 900

 !  Now, before and after, so there is missing data on both sides
    nvals = 200
    kvals = 200
    do i=1,nvals
        cnotes3(i) = blanks(1)
        data3(i) = zmissingFlag()
         do j=1,2
            quality3(j,i) = 0 
         end do
        do j=1,4
            notes3(j,i) = 0
        end do
    end do
    do i=49,148
        cnotes3(i) = cnotes1(i-48)
         data3(i) = data1(i-48)
          do j=1,2
            quality3(j,i) = quality1(j,i-48) 
          end do
        do j=1,4
            notes3(j,i) = notes1(j,i-48)
        end do
    end do

    max = 4
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesBasic nvals stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesBasic lenValuesRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotesBasic lenQuality stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesBasic lenNotesRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(1500,totalNotesRead, 'test_RTS_CharNotesBasic totalNotesRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesBasic units, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesBasic type, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotesBasic precision, loc 70', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesBasic Values, loc 70', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesBasic Quality, loc 70', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotesBasic cNotes, loc 70', status)
    if (status.ne.0) go to 900
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('test_RTS_CharNotesBasic Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in test_RTS_CharNotesBasic') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 1      Format('test_RTS_CharNotesBasic FAILED')
 910    Continue
        if (status.eq.0) status = -1
        return
        end