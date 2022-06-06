    SUBROUTINE test_RTS_Basic(ifltab1, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
      real data1(1010), data2(1010), data3(1010)
      character cpath1*100, cpath2*100
      character cunits*12, ctype*12, ctemp*12
      integer ierr
      integer nvals, i, iofset
      integer maxVals
      integer numberVals, lengthEachValue, lengthEachQuality, lengthEachNote
      integer totalLengthCNotes, lengthUserHeader

      common /lchk/ lcheck
      logical lcheck       

      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_RTS_Basic'
      nvals = 1000
      maxVals = 1000
      do i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)  
      end do
  !  goto 210
!
 1  Format('Basic Time Series test FAILED')   
    cpath1 = '/Basic Time Series/One Value/Flow/date/1Hour/F/'
    nvals = 1
    call zsrts (ifltab1, cpath1, '05JUN1957', '2400', nvals, data1(2), 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    IOFSET = 0
    NVALS = 3
    call zrrts(ifltab1, cpath1, '05JUN1957', '2300', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    data3(1) = zmissingFlag()
    data3(3) = zmissingFlag()
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 5', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 5', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 5', status)
    if (status.ne.0) go to 900


    cpath1 = '/Basic Time Series/1/Flow/date/1Hour/F/'
    data3(1) = data1(1)
    data3(3) = data1(3)
    nvals = 100
    call zsrts (ifltab1, cpath1, '05JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    IOFSET = 0
    NVALS = 100
    call zrrts(ifltab1, cpath1, '05JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 10', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 10', status)
    if (status.ne.0) go to 900
    
    !  Read just a part of what was written
    IOFSET = 0
    NVALS = 24
    call zrrts(ifltab1, cpath1, '06JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 12', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 12', status)
    if (status.ne.0) go to 900
    call checkFloats(data3(25), data2, nvals, 'test_RTS_Basic Values, loc 12', status)
    if (status.ne.0) go to 900
!!!!!!!!!!!!!!!!!!!
!goto 800    
    
    !Re-write in place
    cpath1 = '/Basic Time Series/1/Flow/date/1Hour/F/'
    nvals = 50
    call zsrts (ifltab1, cpath1, '06JUN1957', '2400', nvals, data1(25), 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!  

    IOFSET = 0
    NVALS = 100
    call zrrts(ifltab1, cpath1, '05JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 20', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 20', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 20', status)
    if (status.ne.0) go to 900


    !  Check for writting missing over valid
    cpath1 = '/Basic Time Series/1/Flow/date/1Hour/F/'
    nvals = 2
    data3(25) = zmissingFlag()
    data3(26) = zmissingFlag()
    call zsrts (ifltab1, cpath1, '06JUN1957', '2400', nvals, data3(25), 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!  

    IOFSET = 0
    NVALS = 100
    call zrrts(ifltab1, cpath1, '05JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 30', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 30', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 30', status)
    if (status.ne.0) go to 900    
    !write(messageUnit, *)'Passed loc 30'
    !write(messageUnit, *)' '


    !  How about if we write all missing?
    nvals = 100
    do i=1, nvals
       data1(i) = zmissingFlag()
       data3(i) = data1(i)  
    end do

    call zsrts (ifltab1, cpath1, '05JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    
    call zrrts(ifltab1, cpath1, '05JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

!!!!!!!!!!!!!!!!!!!  FIX ME - status should not be zero here!!!!!
    if (status.gt.7) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    !  Should have no data, nothing to check....
    !call checkString('CFS', cunits, 'test_RTS_Basic units, loc 50', status)
    if (status.ne.0) go to 900
    !call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 50', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 50', status)
    if (status.ne.0) go to 900
    !write(messageUnit, *)'Passed loc 50'
    !write(messageUnit, *)' '



 110    continue
    nvals = 1000
    do i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)  
    end do
 !call zset('mlvl', ' ', 19)
    cpath1 = '/Basic Time Series/2/Flow/date/1Hour/F/'
    call zsrts (ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    IOFSET = 0
    NVALS = 1000
    call zrrts(ifltab1, cpath1, '25JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 60', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 60', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 60', status)
    if (status.ne.0) go to 900
    !write(messageUnit, *)'Passed loc 60'
    !write(messageUnit, *)' '
    

    cpath1 = '/Basic Time Series/2/Flow/01JUL1957/1Hour/F/'
    call ztsGetSizes(ifltab1, cpath1, '', '', '', '', &
    nvals, lengthEachValue, lengthEachQuality, lengthEachNote, &
    totalLengthCNotes, lengthUserHeader, status)  
    if (status.ne.0) go to 900  
    call checkNumbers(744, nvals, 'test_RTS_Basic number stored, loc 70', status)
    if (status.ne.0) go to 900
    call zrrts(ifltab1, cpath1, '', '', nvals, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900   
    call checkNumbers(744,nvals, 'test_RTS_Basic number stored, loc 75', status)
    if (status.ne.0) go to 900
    call checkFloats(data3(122), data2, nvals, 'test_RTS_Basic Values, loc 75', status)
    if (status.ne.0) go to 900
    !write(messageUnit, *)'Passed loc 70'
    !write(messageUnit, *)' '
   

    
    cpath1 = '/Basic Time Series/2/Flow/30JUN1957 - 02AUG1957/1Hour/F/'
    nvals = 1000    
    call zrrts(ifltab1, cpath1, '', '', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900   
    call checkFloats(data3(98), data2, nvals, 'test_RTS_Basic Values, loc 90', status)
    if (status.ne.0) go to 900
    !write(messageUnit, *)'Passed loc 90'
    !write(messageUnit, *)' '

210 nvals = 200
    do i=49, nvals
       data1(i) =FLOAT(i) + 100.
       data3(i) = data1(i)  
    end do

    cpath1 = '/Basic Time Series/2/Flow/date/1Hour/F/'
 !call zset('mlvl', ' ', 15)
    call zsrts (ifltab1, cpath1, '27JUN1957', '2400', nvals, data1(49), 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    IOFSET = 0
    NVALS = 1000
 !call zset('mlvl', ' ', 15)
    call zrrts(ifltab1, cpath1, '25JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 100', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 100', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 100', status)
    if (status.ne.0) go to 900
    !write(messageUnit, *)'Passed loc 100'
    !write(messageUnit, *)' '
   

    nvals = 200
    do i=49, nvals
       data1(i) = zmissingFlag()
       data3(i) = data1(i)  
    end do

    cpath1 = '/Basic Time Series/2/Flow/date/1Hour/F/'
    call zsrts (ifltab1, cpath1, '27JUN1957', '2400', nvals, data1(49), 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    IOFSET = 0
    NVALS = 1000
    call zrrts(ifltab1, cpath1, '25JUN1957', '2400', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 110', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 110', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 110', status)
    if (status.ne.0) go to 900
    !write(messageUnit, *)'Passed loc 110'
    !write(messageUnit, *)' '
   
!
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
    