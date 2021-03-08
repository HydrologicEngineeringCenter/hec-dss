    SUBROUTINE testPairedData(ifltab1, messageUnit, status)
!
!    Test basic writing and reading of Paired Data functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
      real data1(5110), data2(5110)
      double precision ddata1(5110), ddata2(5110)
      character cpath1*100
      character*12 unitsIndependent, typeIndependent, unitsDependent, typeDependent, ctemp
      character labels(100)*50
      character labelsRead(100)*50
      integer ipos, iplan
      integer nvals, i, nOrdinates, n, j, ncurves
      integer numberOrdinates, numberCurves, iHorizontal
      integer userHeader(10), numberUserHeaderRead
      logical labelsStored

      common /lchk/ lcheck
      logical lcheck

!
      WRITE(messageUnit, *)'Begin testPairedData'
      nOrdinates = 10
      do 20 i=1, nOrdinates
       data1(i) =FLOAT(i)       
 20  continue

    do 30 i=11, 20
       data1(i) = FLOAT(i) * 100.0 
 30  continue
  ! 
!
 1  Format('Paired Data test FAILED') 

    !goto 530
    cpath1 = '/Paired Data/One Curve/Stage-Flow/F/No Labels/F/'

    call zspd (ifltab1, cpath1, nOrdinates, 1, 1, 'Feet', 'UNT', 'CFS', 'UNT', data1, '', 0, userHeader, 0, 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

!   Check   
    call zrpd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        data2, 1000, nvals, labels, 0, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(10, numberOrdinates, 'testPairedData numberOrdinates stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, numberCurves, 'testPairedData numberCurves stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 10', status)
    if (status.ne.0) go to 900
     call checkFloats(data1, data2, nvals, 'testPairedData Values, loc 10', status)
    if (status.ne.0) go to 900
    !write(messageUnit, *)'Passed loc 10'
    !write(messageUnit, *)' '

    ! test single curve with a label
    cpath1 = '/Paired Data/One Curve/Stage-Flow/F/With Labels/F/'
    labels(1) = "My Label"    
    call zspd (ifltab1, cpath1, nOrdinates, 1, 1, 'Feet', 'UNT', 'CFS', 'UNT', data1, labels, .true., userHeader, 0, 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

!   Check   
    call zrpd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        data2, 1000, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, numberCurves, 'testPairedData numberCurves stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 20', status)
    if (status.ne.0) go to 900
     call checkFloats(data1, data2, nvals, 'testPairedData Values, loc 20', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 20'
        status = -1
        go to 900
    endif
   call checkString(labels(1), labelsRead(1), 'testPairedData labels, loc 20', status)
    if (status.ne.0) go to 900



   ! test 50 curves with a labels
    cpath1 = '/Paired Data/Fifty Curves/Stage-Flow/F/With Labels/Floats/'
    ncurves = 50
    nOrdinates = 100
    do 200 i=1,(ncurves+1)
    write (labels(i), 199) i
199 format('Curve ', I2)
    do 200 j=1,nOrdinates
        n = j + ((i-1) *nOrdinates )
        data1(n) = (i * 1000) + j
        ddata1(n) = data1(n)
 200  continue    
    call zspd (ifltab1, cpath1, nOrdinates, ncurves, 1, 'Feet', 'UNT', 'CFS', 'UNT', data1, labels, .true.,&
     userHeader, 0, 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

!   Check   
    call zrpd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        data2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 30', status)
    if (status.ne.0) go to 900
     call checkFloats(data1, data2, nvals, 'testPairedData Values, loc 30', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 20'
        status = -1
        go to 900
    endif
    do 220 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 30', status)
        if (status.ne.0) go to 900
220 continue
   
  
     !   Read floats as doubles 
   cpath1 = '/Paired Data/Fifty Curves/Stage-Flow/F/With Labels/Floats/' 
    call zrpdd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        ddata2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 35', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 35', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 35', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 35', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 35', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 35', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 35', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 35', status)
    if (status.ne.0) go to 900
     call checkDoubles(ddata1, ddata2, nvals, 'testPairedData Values, loc 35', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 35'
        status = -1
        go to 900
    endif
    do 222 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 35', status)
        if (status.ne.0) go to 900
222 continue
   
    
   

   ! Now, double precision
    cpath1 = '/Paired Data/Fifty Curves/Stage-Flow/F/With Labels/Doubles/'
    ncurves = 50
    nOrdinates = 100
    do 300 i=1,(ncurves+1)
    write (labels(i), 199) i
    do 300 j=1,nOrdinates
        n = j + ((i-1) *nOrdinates )
        ddata1(n) = (i * 1000) + j
 300  continue    
    call zspdd (ifltab1, cpath1, nOrdinates, ncurves, 1, 'Feet', 'UNT', 'CFS', 'UNT', ddata1, labels, .true.,&
     userHeader, 0, 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

!   Check   
    call zrpdd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        ddata2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 40', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 40', status)
    if (status.ne.0) go to 900
     call checkDoubles(ddata1, ddata2, nvals, 'testPairedData Values, loc 40', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 40'
        status = -1
        go to 900
    endif
    do 320 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 40', status)
        if (status.ne.0) go to 900
320 continue
   
   !   Read doubles as floats 
   cpath1 = '/Paired Data/Fifty Curves/Stage-Flow/F/With Labels/Doubles/'  
    call zrpd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        data2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 50', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 50', status)
    if (status.ne.0) go to 900
     call checkFloats(data1, data2, nvals, 'testPairedData Values, loc 50', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 50'
        status = -1
        go to 900
    endif
    do 420 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 50', status)
        if (status.ne.0) go to 900
420 continue
   
   
   !   Read floats as doubles 
   cpath1 = '/Paired Data/Fifty Curves/Stage-Flow/F/With Labels/Floats/' 
    call zrpdd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        ddata2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 60', status)
    if (status.ne.0) go to 900
     call checkDoubles(ddata1, ddata2, nvals, 'testPairedData Values, loc 60', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 60'
        status = -1
        go to 900
    endif
    do 520 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 60', status)
        if (status.ne.0) go to 900
520 continue


 ! Now, double precision, one curve at at a time
  530   continue
    cpath1 = '/Paired Data/Fifty Curves - one at a time/Stage-Flow/F/With Labels/Doubles/'
    ncurves = 50
    nOrdinates = 100
    do 540 i=1,(ncurves+1)
    write (labels(i), 199) i
    do 540 j=1,nOrdinates
        n = j + ((i-1) *nOrdinates )
        ddata1(n) = (i * 1000) + j
 540  continue   
    iplan = 10 
    call zspdd (ifltab1, cpath1, nOrdinates, ncurves, 1, 'Feet', 'UNT', 'CFS', 'UNT', ddata1, labels, .true., &
     userHeader, 0, iplan, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    do 560 i=1,ncurves
        iplan = 11 
        ipos = (i * nOrdinates) + 1
        call zspdd(ifltab1, cpath1, nOrdinates, i, 1, '', '', '', '', ddata1(ipos), labels(i), .true., &
        userHeader, 0, iplan, status)
        if (status.ne.0) go to 900
        call zinqir(ifltab1, 'error', ctemp, status)
        if (status.ne.0) go to 900
 560  continue

!   Check   
    call zrpdd(ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        ddata2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 140', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 140', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 140', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 140', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 140', status)
    if (status.ne.0) go to 900
     call checkDoubles(ddata1, ddata2, nvals, 'testPairedData Values, loc 140', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 140'
        status = -1
        go to 900
    endif
    do 580 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 140', status)
        if (status.ne.0) go to 900
580 continue

    !  Now with extended labels
    cpath1 = '/Paired Data/Fifty Curves - one at a time/Stage-Flow/F/Extended Labels/Doubles/'
    ncurves = 50
    nOrdinates = 100
    do 740 i=1,(ncurves+1)
    write (labels(i), 730) i
730  format('Extended label for this curve, number ',I2.2)
    do 740 j=1,nOrdinates
        n = j + ((i-1) *nOrdinates )
        ddata1(n) = (i * 1000) + j
        data1(n) = ddata1(n)
 740  continue   
    iplan = 10 
    call zspdd (ifltab1, cpath1, nOrdinates, ncurves, 1, 'Feet', 'UNT', 'CFS', 'UNT', ddata1, labels, .true.,&
     userHeader, 0, iplan, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

    do 760 i=1,ncurves
        iplan = 11 
        ipos = (i * nOrdinates) + 1
        call zspdd(ifltab1, cpath1, nOrdinates, i, 1, '', '', '', '', ddata1(ipos), labels(i), .true., userHeader, 0, iplan, status)
        if (status.ne.0) go to 900
        call zinqir(ifltab1, 'error', ctemp, status)
        if (status.ne.0) go to 900
 760  continue

!   Check   
    call zrpdd(ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        ddata2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 150', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 150', status)
    if (status.ne.0) go to 900
     call checkDoubles(ddata1, ddata2, nvals, 'testPairedData Values, loc 150', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 150'
        status = -1
        go to 900
    endif
    do 780 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 150', status)
        if (status.ne.0) go to 900
780 continue
   
   !   Read doubles as floats  
    call zrpd (ifltab1, cpath1, numberOrdinates, numberCurves, iHorizontal, &
        unitsIndependent, typeIndependent, unitsDependent, typeDependent, &
        data2, 5110, nvals, labelsRead, 50, labelsStored, &
        userHeader, 0, numberUserHeaderRead, status)
        
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call checkNumbers(nOrdinates, numberOrdinates, 'testPairedData numberOrdinates stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(ncurves, numberCurves, 'testPairedData numberCurves stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(1, iHorizontal, 'testPairedData iHorizontal stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkNumbers(5100, nvals, 'testPairedData iHorizontal stored, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('Feet', unitsIndependent, 'testPairedData units 1, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeIndependent, 'testPairedData type 1, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('CFS', unitsDependent, 'testPairedData units 2, loc 160', status)
    if (status.ne.0) go to 900
    call checkString('UNT', typeDependent, 'testPairedData type 2, loc 160', status)
    if (status.ne.0) go to 900
     call checkFloats(data1, data2, nvals, 'testPairedData Values, loc 160', status)
    if (status.ne.0) go to 900
    if (.not.labelsStored) then
        write(messageUnit, *)'Labels stored false; failed read, testPairedData, loc 160'
        status = -1
        go to 900
    endif
    do 590 i=1,ncurves
        call checkString(labels(i), labelsRead(i), 'testPairedData labels, loc 160', status)
        if (status.ne.0) go to 900
590 continue
   
   


!
 800   Continue       
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Paired Data Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Paired Data') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end