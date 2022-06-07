    SUBROUTINE test_RTS_CharNotes(ifltab1, messageUnit, status)
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
      double precision data1(1100), data2(1100), data3(1100)
      integer quality1(2,1100), notes1(4,1100)
      integer quality2(2,1100), notes2(4,1100)
      integer quality3(2,1100), notes3(4,1100)
      character cnotes1(1100)*50
      character cnotes2(1100)*50
      character cnotes3(1100)*50
      character c25*50
      character c26*50
      character c27*50
      integer notes(1)
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12
      integer precision
      integer nvals, kvals, i, j, iofset, max, zero
      integer lenQuality, lenNotes
      integer length
      integer numberVals, lengthEachValue, lengthEachQuality, lengthEachNote
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer totalLengthCNotes
      integer maxVals
      logical lowerCase
      integer lengthUserHeader
!
      integer userHeader(1)
      double precision coordinates(3)
      integer icoord(6) 
      integer k,n,ich 

      common /lchk/ lcheck
      logical lcheck       
      
      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_RTS_CharNotes regular'   
      lenQuality = 2
      lenNotes = 0
      maxVals = 1010
      lowercase = .false.

      nvals = 1000
      ich = ichar('A')
      do i=1, nvals
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
    c25 = cnotes1(25)
    c26 = cnotes1(26)
    c27 = cnotes1(27)
!
 1  Format('Time Series Notes test FAILED')   
   !call zset('mlvl', ' ', 12)
!
! 
    cpath1 = '/Time Series With Notes/Regular 1/Flow/date/1Hour/F/'

   ! cnotes1(1) = ' '
   ! cnotes1(2) = 'Gage was damaged during car accident'
   ! cnotes1(3) = 'George fixed, but not calibrated'
   ! cnotes1(4) = 'Frank calibrated'
   ! cnotes1(5) = ' '
    
    do i=1,nvals
      cnotes3(i) = cnotes1(i)
    end do
    
    call ztsRegStoreFull (ifltab1, cpath1, '25JUN1957', '2400', nvals, &
    data1, 2, quality1, 2, notes, 0, cnotes1, 1, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    kvals = nvals


    IOFSET = 0  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes Regular nvals stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes lenValuesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotes lenQuality stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes lenNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(14000,totalNotesRead, 'test_RTS_CharNotes totalNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes Regular units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes Regular type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotes Regular precision, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Regular Values, loc 10', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotes Regular Quality, loc 10', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 10', status)
    if (status.ne.0) go to 900   


   !  Check getNumberValues function
   call ztsGetSizes(ifltab1, cpath1, '28JUN1957', '1400', '04JUL1957', '0800', &
    numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(1464, numberVals, 'test_RTS_CharNotes Regular ztsGetSizes number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachValue, 'test_RTS_CharNotes Regular ztsGetSizes lengthEachValue, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachQuality, 'test_RTS_CharNotes Regular ztsGetSizes lengthEachQuality, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0, lengthEachNote, 'test_RTS_CharNotes Regular ztsGetSizes lengthEachNote, loc 20', status)
    if (status.ne.0) go to 900

    

    lenQuality = 2
    lenNotes = 4
!
      
      ich = ichar('A')
      do i=1, 1100
       data1(i) = FLOAT(i)
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
        quality1(j,i) = (i*100) + j
        quality3(j,i) = quality1(j,i) 
       end do
        do j=1,4
        notes1(j,i) = (i*10000) + j
        notes3(j,i) = notes1(j,i)        
        end do
      end do
 
     do i=1,1100
      cnotes3(i) = cnotes1(i)
     end do
! 
    cpath1 = '/Time Series With Notes/Regular 2/Flow/date/1Hour//'
    
    nvals = 100
    
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes1, (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 1,  '',  0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    IOFSET = 0
    KVALS = nvals
    max = 4
    zero = 0

    !  First - check same exact data
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '10JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes nvals stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes lenValuesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_RTS_CharNotes Regular precision, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Values, loc 30', status)
    if (status.ne.0) go to 900 
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotes lenQualityRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes lenNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(1400,totalNotesRead, 'test_RTS_CharNotes totalNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 30', status)
    if (status.ne.0) go to 900
       
    !  Store 3 new notes one day later (24 vals)
    c25 = cnotes1(25)
    c26 = cnotes1(26)
    c27 = cnotes1(27)
    nvals = 3
    cnotes1(25) = 'this is the first of three lines'
    cnotes1(26) = 'A second line'
    cnotes1(27) = 'The third line for these notes'
    cnotes3(25) = cnotes1(25)
    cnotes3(26) = cnotes1(26)
    cnotes3(27) = cnotes1(27)
    call ztsRegStoreFull (ifltab1, cpath1, '11JUN1987', '2400', nvals, &
    data1(25), 2, quality1(1,25), 2, notes1, 0, cnotes1(25), (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '',  0, status)
     
    !  This should contain same data as first, except for different notes
    nvals = kvals
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
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
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes nvals stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes lenValuesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotes lenQuality stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes lenNotesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(1447,totalNotesRead, 'test_RTS_CharNotes totalNotesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes units, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes type, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotes Regular precision, loc 40', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Values, loc 40', status)
    if (status.ne.0) go to 900 
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 40', status)
    if (status.ne.0) go to 900
    
    !  Now, don't store notes for those three days.  They should not change and still be there
    !  updated.  Need to store nulls to change, or delete data set.
    call ztsRegStoreFull (ifltab1, cpath1, '11JUN1987', '2400', nvals, &
    data1(25), 2, quality1(1,25), 2, notes1, 0, cnotes1(25), 0, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '',  0, status)
     
    !  This should contain same data as first, except for different notes
    nvals = kvals
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
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
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes nvals stored, loc 45', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes lenValuesRead stored, loc 45', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotes lenQuality stored, loc 45', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes lenNotesRead stored, loc 45', status)
    if (status.ne.0) go to 900
    call checkNumbers(1447,totalNotesRead, 'test_RTS_CharNotes totalNotesRead stored, loc 45', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes units, loc 45', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes type, loc 45', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotes Regular precision, loc 45', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Values, loc 45', status)
    if (status.ne.0) go to 900 
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 45', status)
    if (status.ne.0) go to 900

    
    !  Now don't store notes one day later (24 vals)
    nvals = 3
    cnotes1(25) = ''
    cnotes1(26) = ''
    cnotes1(27) = ''
    cnotes3(25) = cnotes1(25)
    cnotes3(26) = cnotes1(26)
    cnotes3(27) = cnotes1(27)
    call ztsRegStoreFull (ifltab1, cpath1, '11JUN1987', '2400', nvals, &
    data1(25), 2, quality1(1,25), 2, notes1, 0, cnotes1(25), (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '',  0, status)

    !  This should contain same data as first, except for different notes
    nvals = kvals
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
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
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes nvals stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes lenValuesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotes lenQuality stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes lenNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(1372,totalNotesRead, 'test_RTS_CharNotes totalNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotes Regular precision, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Values, loc 50', status)
    if (status.ne.0) go to 900 
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 50', status)
    if (status.ne.0) go to 900

    cnotes1(25) = c25
    cnotes1(26) = c26
    cnotes1(27) = c27
    cnotes3(25) = cnotes1(25)
    cnotes3(26) = cnotes1(26)
    cnotes3(27) = cnotes1(27)
    
    !  Now write the data set with notes, and then without notes
    !  Under this case, the notes should be not be changed on the second write
    nvals = 100
    cpath1 = '/Time Series With Notes/Regular 3/Flow/date/1Hour//'
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes1, (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 3, '',  0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    IOFSET = 0
    KVALS = nvals
    max = 4
    zero = 0

    !  check same exact data
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
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
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes nvals stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes lenValuesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotes lenQuality stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes lenNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(1400,totalNotesRead, 'test_RTS_CharNotes totalNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes type, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(3, precision, 'test_RTS_CharNotes Regular precision, loc 60', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Values, loc 60', status)
    if (status.ne.0) go to 900 
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 60', status)
    if (status.ne.0) go to 900
    
    !  Now write without notes
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes1, 0, userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '',  0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    !  And check
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
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
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotes number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(100,totalNotesRead, 'test_RTS_CharNotes number stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes units, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes type, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotes Regular precision, loc 70', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Values, loc 70', status)
    if (status.ne.0) go to 900 
    !!!!!!!!!!!!!!!!!!!   FIX ME - lenQualityRead returned 2, lenNotesRead 0
    !!!!!!!!!!!!!!!!!!!!  NOT consistent!!!!

  
    
    !  Now store the full array
    nvals = 1000
    cpath1 = '/Time Series With Notes/Regular 4/Flow/date/1Hour//'
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes1, (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 1,  '',  0, status)
    
    nvals = 1000
    kvals = nvals
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality,lenQualityRead,  notes, 0, lenNotesRead,  &
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
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotes nvals stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotes lenValuesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQuality, 'test_RTS_CharNotes lenQuality stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotes lenNotesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(14000,totalNotesRead, 'test_RTS_CharNotes totalNotesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotes units, loc 100', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotes type, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_RTS_CharNotes Regular precision, loc 100', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotes Values, loc 100', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 100', status)
    if (status.ne.0) go to 900
   
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Regular Regular Time Series with cnotes Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Regular Regular Time Series with cnotes ') 
        endif         
       return
!
900 Continue
    call zclose(ifltab1)
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end