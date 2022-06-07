    SUBROUTINE test_RTS_CharNotesFull(ifltab1, messageUnit, status)
!
!    This test writing notes to a record without notes,
!    and then writing without notes to a record that has them
!
!   In general, one should always write exactly what the final record should contain
!   If you write something with different sets of data (i.e., quality to a record
!   without quality), what is written "quasi-undefiend", i.e., do not do that 
!   and do not expect a consistent result.
!   However, DSS will try to preserve data and do what it thinks is best, and
!   this action generates a minor warning that is not to the level to print a message
!
!   If you write notes to a record without notes, notes will be added to all data in that record
!   If you write data without notes to a record with notes, the notes will not be changed.
!   However, do not rely on this (you should first read the notes and then re-store them).
!
!   If you want to delete notes from a record, you should re-write the entire record, as a
!   full re-write will replace the entire record.
!   One way to do this would be to read the full record and write back without notes (or quality)
!   If you only did this to a partial record, then the notes would not be removed.
!
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
    !  Write to DSS array 1, read into array 2, compare with array 3
      !double precision data1(1100), data2(1100), data3(1100)
      double precision data1(1100), data2(1100), data3(1100)
      integer quality1(2,1100), notes1(4,1100)
      integer quality2(2,1100), notes2(4,1100)
      integer quality3(2,1100), notes3(4,1100)
      character cnotes1(1100)*50
      character cnotes2(1100)*50
      character cnotes3(1100)*50
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12
      integer precision
      integer nvals, kvals, i, j, iofset, max, zero, n, k, ich
      integer length
      integer lenData, lenQuality, lenNotes 
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer maxVals
      logical lowerCase
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
     ! call zset('mlvl', ' ', 10)
      WRITE(messageUnit, *)'Begin test_RTS_CharNotesFull'
      maxVals = 1010   

    lenQuality = 2
    lenNotes = 4
    lowercase = .false.
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
    cpath1 = '/Time Series With Notes/Full/Flow/date/1Hour//'
    
    nvals = 100
    !call zset('mlvl', '', 13)
    
    call ztsRegStoreFull (ifltab1, cpath1, '10JUN1987', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes1, (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '',  0, status)

    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    IOFSET = 0
    KVALS = nvals
    max = 4
    zero = 0

    !  First - check same exact data
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
    call ztsRetrieveRegArgs(ifltab1, cpath1, '10JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull nvals stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesFull lenValuesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotesFull precision, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 10', status)
    if (status.ne.0) go to 900 
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotesFull lenQualityRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesFull lenNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1400,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotesFull cNotes, loc 10', status)
    if (status.ne.0) go to 900
    
    

      do  i=1,nvals
        data3(i) = zmissingFlag()
      end do

    !  Now, before...  all data should be missing, notes blank
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenData, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '02JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenData, quality2, 2, lenQuality, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull nvals stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenData, 'test_RTS_CharNotesFull lenData stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQuality, 'test_RTS_CharNotesFull lenQuality stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotes, 'test_RTS_CharNotesFull lenNotes stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(100,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotesFull precision, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 20', status)
    if (status.ne.0) go to 900
    call checkBlanks(cnotes2, nvals, 'test_RTS_CharNotesFull cNotes not Blank, loc 20', status)
    if (status.ne.0) go to 900


 !  Now, after...  all data should be missing
     call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '20JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenData, quality2, 2, lenQuality, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull nvals stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenData, 'test_RTS_CharNotesFull lenData stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQuality, 'test_RTS_CharNotesFull lenQuality stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotes, 'test_RTS_CharNotesFull lenNotes stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(100,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotesFull precision, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 30', status)
    if (status.ne.0) go to 900
    call checkBlanks(cnotes2, nvals, 'test_RTS_CharNotesFull cNotes not Blank, loc 30', status)
    if (status.ne.0) go to 900



 !  Now, partly before, so 1/2 of data is there, the other missing
    do i=49,100
        data3(i) = data1(i-48)
    end do
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenData, quality2, 2, lenQuality, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, &
    userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull nvals stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenData, 'test_RTS_CharNotesFull lenData stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQuality, 'test_RTS_CharNotesFull lenQuality stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotes, 'test_RTS_CharNotesFull lenNotes stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(753,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotesFull precision, loc 40', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 40', status)
    if (status.ne.0) go to 900
    call checkBlanks(cnotes2, 48, 'test_RTS_CharNotesFull cNotes not Blank, loc 40', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2(49), 52, 'test_RTS_CharNotesFull cNotes, loc 40', status)
    if (status.ne.0) go to 900
   

 !  Now, partly after, so 1/2 of data is there, the other missing
    nvals = 110
    kvals = 110
    do i=1,52
        data3(i) = data1(i+48)
    end do
    do i=53,nvals
         data3(i) = zmissingFlag()
    end do
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenData, quality2, 2, lenQuality, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, &
    userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull nvals stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenData, 'test_RTS_CharNotesFull lenData stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQuality, 'test_RTS_CharNotesFull lenQuality stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotes, 'test_RTS_CharNotesFull lenNotes stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(809,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotesFull precision, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 50', status)
    if (status.ne.0) go to 900
    call checkBlanks(cnotes2(53), 48, 'test_RTS_CharNotesFull cNotes not Blank, loc 50', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(49), cnotes2, 52, 'test_RTS_CharNotesFull cNotes, loc 50', status)
    if (status.ne.0) go to 900

 !  Now, in the middle, so we are only reading valid data
    nvals = 50
    kvals = nvals
    do i=1,nvals
        data3(i) = data1(i+48)
    end do
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '12JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenData, quality2, 2, lenQuality, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, &
    userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull nvals stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenData, 'test_RTS_CharNotesFull lenData stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQuality, 'test_RTS_CharNotesFull lenQuality stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotes, 'test_RTS_CharNotesFull lenNotes stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(700,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotesFull precision, loc 60', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 60', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3(49), cnotes2, nvals, 'test_RTS_CharNotesFull cNotes, loc 60', status)
    if (status.ne.0) go to 900


 !  Now, before and after, so there is missing data on both sides
    nvals = 200
    kvals = 200
    do i=1,nvals
        data3(i) = zmissingFlag()
    end do
    do i=49,148
         data3(i) = data1(i-48)
    end do
    call ztsRegRetClear(ifltab1, cpath1, '10JUN1987', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '08JUN1987', '2400', iofset, kvals, nvals,  &
    data2, 2, lenData, quality2, 2, lenQuality, notes2, 0, lenNotes, &
    cnotes2, (50*nvals), totalNotesRead, &
    userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull nvals stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenData, 'test_RTS_CharNotesFull lenData stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQuality, 'test_RTS_CharNotesFull lenQuality stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotes, 'test_RTS_CharNotesFull lenNotes stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(1500,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 70', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 70', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, precision, 'test_RTS_CharNotesFull precision, loc 70', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 70', status)
    if (status.ne.0) go to 900
    call checkBlanks(cnotes2, 48, 'test_RTS_CharNotesFull cNotes not Blank, loc 70', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2(49), 100, 'test_RTS_CharNotesFull cNotes, loc 70', status)
    if (status.ne.0) go to 900
    call checkBlanks(cnotes2(149), 52, 'test_RTS_CharNotesFull cNotes not Blank, loc 70a', status)
    if (status.ne.0) go to 900
    
    
    !!!  Test Normal
    cpath1 = '/Time Series No Missing/With C Notes/Flow/date/1Hour/F/'
    nvals = 1000
    call ztsRegStoreFull (ifltab1, cpath1, '25JUN1957', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes1, (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0,  status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check


    IOFSET = 0
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull number stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesFull lenValuesRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotesFull lenQualityRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesFull lenNotesRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 150', status)
    if (status.ne.0) go to 900
    call checkDoubles(data1, data2, nvals, 'test_RTS_CharNotesFull Values, loc 150', status)
    if (status.ne.0) go to 900    
    call checkNumbers(14000,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 150', status)
    if (status.ne.0) go to 900  

    
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!!!!!!!!!  Test c notes without data (i.e., all missing data)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    
    cpath1 = '/Time Series All Missing/With C Notes/Flow/date/1Hour/F/'
    nvals = 1000
    do i=1,nvals
        data1(i) = zmissingFlag()
        data3(i) = zmissingFlag()
    end do
      
    call ztsRegStoreFull (ifltab1, cpath1, '25JUN1957', '2400', nvals, &
    data1, 2, quality1, 0, notes1, 0, cnotes1, (50*nvals), userHeader, 0, &  
    'CFS', 'PER-AVER', 2, '', 0,  status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check


    IOFSET = 0
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, (50*nvals), totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull number stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesFull lenValuesRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotesFull lenQualityRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesFull lenNotesRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 160', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 160', status)
    if (status.ne.0) go to 900    
    call checkNumbers(14000,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNotes(cnotes3, cnotes2, nvals, 'test_RTS_CharNotes cNotes, loc 160', status)
    if (status.ne.0) go to 900 



    !  Now before any data
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '05JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '05JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
    do j=1,4
        notes3(j,i) = 0
    end do
    end do

    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull number stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesFull lenValuesRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotesFull lenQualityRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesFull lenNotesRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 180', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 180', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 180', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesFull Quality, loc 180', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_CharNotesFull Notes, loc 180', status)
    if (status.ne.0) go to 900    
    
    
    !  Now after any data
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '20aug1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '20aug1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
    do j=1,4
            notes3(j,i) = 0
    end do
    end do

    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull number stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesFull lenValuesRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotesFull lenQualityRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesFull lenNotesRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 190', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 190', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 190', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesFull Quality, loc 190', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_CharNotesFull Notes, loc 190', status)
    if (status.ne.0) go to 900    


    !Now starting before data
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '05JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '05JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do i=1,480
        quality3(1,i) = 0
        quality3(2,i) = 0
     do j=1,4
        notes3(j,i) = 0
     end do
    end do
    do i=481,nvals
        quality3(1,i) = quality1(1,i-480)
        quality3(2,i) = quality1(2,i-480)
     do j=1,4
        notes3(j,i) = notes1(j,i-480)        
     end do
    end do


    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull number stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesFull lenValuesRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotesFull lenQualityRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesFull lenNotesRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 200', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 200', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 200', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesFull Quality, loc 200', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_CharNotesFull Notes, loc 200', status)
    if (status.ne.0) go to 900    
  
  
  
    !Now starting after start of data
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '29JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '29JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes2, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
    do j=1,4
        notes3(j,i) = 0
    end do
    end do
    do i=1,(nvals-96)
        quality3(1,i) = quality1(1,i+96)
        quality3(2,i) = quality1(2,i+96)
    do j=1,4
        notes3(j,i) = notes1(j,i+96)
    end do
    end do


    call checkNumbers(kvals,nvals, 'test_RTS_CharNotesFull number stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_CharNotesFull lenValuesRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_CharNotesFull lenQualityRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_CharNotesFull lenNotesRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_CharNotesFull totalNotesRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_CharNotesFull units, loc 210', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_CharNotesFull type, loc 210', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_CharNotesFull Values, loc 210', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_CharNotesFull Quality, loc 210', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_CharNotesFull Notes, loc 210', status)
    if (status.ne.0) go to 900    
 


!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('test_RTS_CharNotesFull Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in test_RTS_CharNotesFull') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 1      Format('test_RTS_CharNotesFull FAILED')
 910    Continue
        if (status.eq.0) status = -1
        return
        end