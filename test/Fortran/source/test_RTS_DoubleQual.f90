    SUBROUTINE test_RTS_DoubleQual(ifltab1, messageUnit, status)
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
      character cpath1*100, cpath2*100
      character cunits*12, ctype*12, ctemp*12, tzone*25, cnotes*1
      integer ierr, precision
      integer nvals, kvals, i, j, iofset, max, zero
      integer lenQuality, lenNotes 
      integer length, offset, lenCoord, lenIcord
      integer numberVals, lengthEachValue, lengthEachQuality, lengthEachNote
      integer lenValuesRead, lenQualityRead, lenNotesRead, totalNotesRead
      integer totalLengthCNotes
      integer maxVals
      integer lengthUserHeader

!
      integer userHeader(1)
      double precision coordinates(3), rcoordinates(3)
      integer icoord(6), ricoord(6)  

      common /lchk/ lcheck
      logical lcheck       
      
      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_RTS_DoubleQual'  
      maxVals = 1010 
      lenQuality = 2
      lenNotes = 4

      nvals = 1000
      do 20 i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)
       do 10 j=1,2
        quality3(j,i) = 0
 10    continue 
        do 20 j=1,4
        notes3(j,i) = 0     
 20  continue
    kvals = nvals
!
 1  Format('Basic Time Series double basic test FAILED')   
    cpath1 = '/Basic Time Series Double/1/Flow/date/1Hour/F/'
    call zsrtsd (ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    call zrrtsd(ifltab1, cpath1, '25JUN1957', '2400', nvals, data2, cunits, ctype, offset, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 10', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 10', status)
    if (status.ne.0) go to 900
    
    
    !  Now test asking for quailty and notes that don't exist - should be 0
    IOFSET = 0
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '25JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)

 
    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 20', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 20', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 20', status)
    if (status.ne.0) go to 900
!
!
!     Now test with quality and notes
 100    continue
      nvals = 1010
      do 120 i=1, nvals
       data1(i) = FLOAT(i)
      if (i.le.10) data1(i) = zmissingFlag()
       data3(i) = data1(i) 
       do 110 j=1,2
        quality1(j,i) = (i*100) + j
        if ((i.gt.100).and.(i.lt.300)) quality1(j,i) = 123
        quality3(j,i) = quality1(j,i) 
 110    continue 
        do 120 j=1,4
        notes1(j,i) = (i*10000) + j
        if ((i.gt.400).and.(i.lt.600)) notes1(j,i) = 456
        notes3(j,i) = notes1(j,i)        
 120  continue
! 
    cpath1 = '/Time Series With Quailty/1/Flow/date/1Hour/F/'
    nvals = 1000
    
    
    call ztsRegStoreFull (ifltab1, cpath1, '25JUN1957', '2400', nvals, &
    data1, 2, quality1, 2, notes1, 4, cnotes, 0, userHeader, 0, &  
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
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 30', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 30', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 30', status)
    if (status.ne.0) go to 900


   !  Check getNumberValues function
   call ztsGetSizes(ifltab1, cpath1, '28JUN1957', '1400', '04JUL1957', '0800', &
    numberVals, lengthEachValue, lengthEachQuality, lengthEachNote, totalLengthCNotes, lengthUserHeader, status)
     if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(1464, numberVals, 'test_RTS_DoubleQual ztsGetSizes number stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachValue, 'test_RTS_DoubleQual ztsGetSizes lengthEachValue, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(2, lengthEachQuality, 'test_RTS_DoubleQual ztsGetSizes lengthEachQuality, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(4, lengthEachNote, 'test_RTS_DoubleQual ztsGetSizes lengthEachNote, loc 40', status)
    if (status.ne.0) go to 900

    


    !  Now test with time zone and coordinate info...
    nvals = 1000 
    cpath1 = '/Time Series With Coords/1/Flow/date/1Hour/F/'  
    coordinates(1) = 1.0
    coordinates(2) = 2.0
    coordinates(3) = 3.0
    icoord(1) = 1
    icoord(2) = 2
    icoord(3) = 3
    icoord(4) = 4
    icoord(5) = 5
    icoord(6) = 6
    call ztsRegStoreFull (ifltab1, cpath1, '25JUN1957', '2400', nvals, &
    data1, 2, quality1, 2, notes1, 4, cnotes, 0, userHeader, 0, &  
    'CFS', 'PER-AVER', 1, 'Pacific Time', 0, status)
    
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call zlocationStore(ifltab1, cpath1, &				
				coordinates, icoord, & 
				'Pacific Time', '', status)
    
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
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    
 
    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call zlocationretrieve(ifltab1, cpath1,	 &			
				rcoordinates, &			
				ricoord, tzone, ctemp, status)  
    write(messageUnit,*)'FIX ME'
    go to 2
	if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    call checkNumbers(3,lenCoord, 'test_RTS_DoubleQual number coordinates, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(6,lenIcord, 'test_RTS_DoubleQual number coordinats description, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(coordinates, rcoordinates, lenCoord, 'test_RTS_DoubleQual Coordinate Values, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(icoord, ricoord, 1, lenIcord, 'test_RTS_DoubleQual coordinate description, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('Pacific Time', tzone, 'test_RTS_DoubleQual time zone name, loc 50', status)
    if (status.ne.0) go to 900

    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, precision, 'test_RTS_DoubleQual precision, loc 50', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 50', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 50', status)
    if (status.ne.0) go to 900
    
    
    
    
    !Now, write out all missing, but with quality
2    cpath1 = '/Time Series With Quailty/All Missing/Flow/date/1Hour/F/'
    nvals = 1000
    do 210 i=1,nvals
        data1(i) = zmissingFlag()
        data3(i) = zmissingFlag()
210 continue    
      
    call ztsRegStoreFull (ifltab1, cpath1, '25JUN1957', '2400', nvals, &
    data1, 2, quality1, 2, notes1, 0, cnotes, 0, userHeader, 0, &  
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
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 60', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 60', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 60', status)
    if (status.ne.0) go to 900



    !  Now before any data
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '05JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '05JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do 220 i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
220 continue 

    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 80', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 80', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 80', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 80', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 80', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 80', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 80', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 80', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 80', status)
    if (status.ne.0) go to 900
    
    
    !  Now after any data
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '20aug1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '20aug1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
        do 230 i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
230 continue

    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 90', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 90', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 90', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 90', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 90', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 90', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 90', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 90', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 90', status)
    if (status.ne.0) go to 900


    !Now starting before data
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '05JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '05JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do 240 i=1,480
        quality3(1,i) = 0
        quality3(2,i) = 0
240 continue
    do 250 i=481,nvals
        quality3(1,i) = quality1(1,i-480)
        quality3(2,i) = quality1(2,i-480)
250 continue


    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 100', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 100', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 100', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 100', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 100', status)
    if (status.ne.0) go to 900
  
  
  
    !Now starting after start of data
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '29JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '29JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do 260 i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
260 continue
    do 270 i=1,(nvals-96)
        quality3(1,i) = quality1(1,i+96)
        quality3(2,i) = quality1(2,i+96)
270 continue


    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 110', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 110', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 110', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 110', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 110', status)
    if (status.ne.0) go to 900
 
 
 
 !!!!!!!!!!!!!!!!
   
    
    
    !Now, write out all missing, but with quality and notes
    cpath1 = '/Time Series With Quailty And Notes/All Missing/Flow/date/1Hour/F/'
    nvals = 1000
    do 310 i=1,nvals
        data1(i) = zmissingFlag()
        data3(i) = zmissingFlag()
        quality3(1,i) = quality1(1,i)
        quality3(2,i) = quality1(2,i)
310 continue    
      
    call ztsRegStoreFull (ifltab1, cpath1, '25JUN1957', '2400', nvals, &
    data1, 2, quality1, 2, notes1, 4, cnotes, 0, userHeader, 0, &  
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
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '25JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 160', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 160', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 160', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 160', status)
    if (status.ne.0) go to 900



    !  Now before any data
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '05JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '05JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do 420 i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
    do 420 j=1,4
        notes3(j,i) = 0
420 continue 

    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 180', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 180', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 180', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 180', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 180', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 180', status)
    if (status.ne.0) go to 900    
    
    
    !  Now after any data
    NVALS = 100 
    KVALS = 100  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '20aug1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '20aug1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do 430 i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
    do 430 j=1,4
            notes3(j,i) = 0
430 continue

    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 190', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 190', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 190', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 190', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 190', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 190', status)
    if (status.ne.0) go to 900    


    !Now starting before data
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '05JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '05JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do 440 i=1,480
        quality3(1,i) = 0
        quality3(2,i) = 0
     do 440 j=1,4
        notes3(j,i) = 0
440 continue
    do 450 i=481,nvals
        quality3(1,i) = quality1(1,i-480)
        quality3(2,i) = quality1(2,i-480)
     do 450 j=1,4
        notes3(j,i) = notes1(j,i-480)        
450 continue


    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 200', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 200', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 200', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 200', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 200', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 200', status)
    if (status.ne.0) go to 900    
  
  
  
    !Now starting after start of data
    NVALS = 1000 
    KVALS = 1000  
    max = 4
    zero = 0
    call ztsRegRetClear(ifltab1, cpath1, '29JUN1957', '2400', kvals, nvals, iofset, &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, coordinates, zero, length, &
    icoord, zero, length, ctemp, status)
    

    call ztsRetrieveRegArgs(ifltab1, cpath1, '29JUN1957', '2400', iofset, kvals, nvals,  &
    data2, 2, lenValuesRead, quality2, lenQuality, lenQualityRead, notes2, 4, lenNotesRead, &
    cnotes, 0, totalNotesRead, userHeader, zero, length, &
    cunits, ctype, precision, ctemp, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    do 460 i=1,nvals
        quality3(1,i) = 0
        quality3(2,i) = 0
    do 460 j=1,4
        notes3(j,i) = 0
460 continue
    do 470 i=1,(nvals-96)
        quality3(1,i) = quality1(1,i+96)
        quality3(2,i) = quality1(2,i+96)
    do 470 j=1,4
        notes3(j,i) = notes1(j,i+96)
470 continue


    call checkNumbers(kvals,nvals, 'test_RTS_DoubleQual number stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenValuesRead, 'test_RTS_DoubleQual lenValuesRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(2,lenQualityRead, 'test_RTS_DoubleQual lenQualityRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(4,lenNotesRead, 'test_RTS_DoubleQual lenNotesRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkNumbers(0,totalNotesRead, 'test_RTS_DoubleQual totalNotesRead stored, loc 210', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_DoubleQual units, loc 210', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_DoubleQual type, loc 210', status)
    if (status.ne.0) go to 900
    call checkDoubles(data3, data2, nvals, 'test_RTS_DoubleQual Values, loc 210', status)
    if (status.ne.0) go to 900
    call checkInts(quality3, quality2, lenQuality, nvals, 'test_RTS_DoubleQual Quality, loc 210', status)
    if (status.ne.0) go to 900
    call checkInts(notes3, notes2, lenNotes, nvals, 'test_RTS_DoubleQual Notes, loc 210', status)
    if (status.ne.0) go to 900    
 
   
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Basic Regular Time Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Basic Regular Time Series') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end