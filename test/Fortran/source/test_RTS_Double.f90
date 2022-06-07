   SUBROUTINE test_RTS_Double(ifltab1, messageUnit, status)
!
!    Test extended writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
      double precision data1(1000), data2(1000), data3(1000)
      character cpath1*100, cpath2*100
      character cunits*12, ctype*12, ctemp*12
      integer ierr
      integer nvals, i, j, iofset, number 
      integer numberValuesRead
      integer maxVals

      common /lchk/ lcheck
      logical lcheck
      
      real zmissingflag
	  external zmissingflag
!
!
!     Test Expansion
!
      WRITE(messageUnit, *)'Begin test_RTS_Double'
!
  1  Format('Double Regular Time Series test FAILED')
!  
      nvals = 1000
      maxVals = 1000
      do i=1, nvals
       data1(i) = i
      end do
 
    data1(1)= zmissingFlag()
	data1(2)= zmissingFlag()
	data1(3)= zmissingFlag()
	data1(4)= zmissingFlag()
	data1(5)= zmissingFlag()
	data1(6)= zmissingFlag()
	data1(7)= zmissingFlag()
	data1(8)= zmissingFlag()
	data1(9)= zmissingFlag()
	data1(10) = zmissingFlag()
	data1(11)= 6.
	data1(12)= 6.
	data1(13)= 6.
	data1(14)= 4.
	data1(15)= 8.
	data1(16) = 3.
	data1(101)= 4.
	data1(102)= 4.
	data1(103)= 7.
	data1(104)= 6.
	data1(105)= 6.
	data1(106)= 6.
	data1(107)= 6.
	data1(108)= 4.
	data1(109)= 8.
	data1(110) = 3.
	data1(111)= 6.
	data1(112)= 6.
	data1(113)= 6.
	data1(114)= 4.
	data1(115)= 8.
	data1(116) = 3.
	
	do i=200, 400
	  data1(i) = 10.0
    end do
 
    do i=950, 1000
	  data1(i) = zmissingFlag()
    end do
!
    do i=1, nvals
       data3(i) = data1(i)
    end do
 
    nvals = 1000
    cpath1 = '/Double Regular Time Series/1/Flow//1Hour/F/'    
    call zsrtsd (ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) then
        Write(messageUnit, *)'first zsrts call failed'
        go to 900
    endif
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Internal failure after first zsrts call'
        go to 900
    endif
!   Check
!   
    IOFSET = 0
    number = nvals
    call zrrtsd(ifltab1, cpath1, '25JUN1957', '2400', number, data2, cunits, ctype, IOFSET, status)
    if (status.lt.0) then
        Write(messageUnit, *)'first zrrts call failed'
        go to 900
    endif
    if (status.gt.4) then
        Write(messageUnit, *)'first zrrts call failed'
        go to 900
    endif
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Internal failure after first zrrts call'
        go to 900
    endif
    if (nvals.ne.number) then
        write(messageUnit, 1)
        Write(messageUnit, *)'Number of values not correct: ',number
        go to 910
    endif
     do i=1,nvals
        if (data2(i).ne.data3(i)) then
             Write(messageUnit, *)'first zrrts call failed'
            write(messageUnit, 1)
            write(messageUnit, 45) 
            write(messageUnit, 46) i, data2(i), data3(i)
 45         Format('Values read do not match those written')
 46         Format('At ordinate: ', I8, ' Values: ', 2F8.2)
            go to 900
        endif
     end do
!
    if (cunits(1:3).ne.'CFS') Then
        write(messageUnit, 1)
        write(messageUnit, 61) cunits(1:3)
 61     format('Units do not match: ', A)
        go to 910
    endif
!
    if (ctype(1:8).ne.'PER-AVER') Then
        write(messageUnit, 1)
        write(messageUnit, 62) ctype(1:8)
 62     format('Type does not match: ', A)
        go to 910
    endif
!
!
     nvals = 18
!
    cpath1 = '/Double Time Series/2/Flow//1Hour/F/'
    call zsrtsd(ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Second zsrts call failed'
        go to 900
    endif
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Internal failure after second zsrts call'
        go to 900
    endif
!   Check

    IOFSET = 0
    number = nvals
    call zrrtsd(ifltab1, cpath1, '25JUN1957', '2400', number, data2, cunits, ctype, IOFSET, status)
   if (status.lt.0) then
        Write(messageUnit, *)'second zrrts call failed'
        go to 900
    endif
    if (status.gt.4) then
        Write(messageUnit, *)'second zrrts call failed'
        go to 900
    endif
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Internal failure after second zrrts call'
        go to 900
    endif
    if (nvals.ne.number) then
        write(messageUnit, 1)
        Write(messageUnit, *)'Number of values not correct: ',number
        go to 910
    endif
     do i=1,nvals
        if (data2(i).ne.data3(i)) then
            Write(messageUnit, *)'Second zsrts call failed'
            write(messageUnit, 1)
            write(messageUnit, 45) 
            write(messageUnit, 46) i, data2(i), data3(i)
            go to 900
        endif
     end do
!
    if (cunits(1:3).ne.'CFS') Then
        write(messageUnit, 1)
        write(messageUnit, 61) cunits(1:3)
        go to 910
    endif
!
    if (ctype(1:8).ne.'PER-AVER') Then
        write(messageUnit, 1)
        write(messageUnit, 62) ctype(1:8)
        go to 910
    endif
!
!
!   Now check for compression being turned off because of expansions
        
    nvals = 20
    cpath1 = '/Double Time Series/3/Flow//1Hour/F/'
    do i=1,10
    nvals = nvals + 5
    call zsrtsd(ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Third zsrts call failed'
        go to 900
    endif
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Internal failure after third zsrts call'
        go to 900
    endif
!   Check

    IOFSET = 0
    number = nvals
    call zrrtsd(ifltab1, cpath1, '25JUN1957', '2400', number, data2, cunits, ctype, IOFSET, status)
    if (status.lt.0) then
        Write(messageUnit, *)'third zrrts call failed'
        go to 900
    endif
    if (status.gt.4) then
        Write(messageUnit, *)'third zrrts call failed'
        go to 900
    endif
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) then
        Write(messageUnit, *)'Internal failure after third zrrts call'
        go to 900
    endif
    if (nvals.ne.number) then
        write(messageUnit, 1)
        Write(messageUnit, *)'Number of values not correct: ',number
        go to 910
    endif
     do j=1,nvals
        if (data2(j).ne.data3(j)) then
            Write(messageUnit, *)'Third zsrts call failed'
            write(messageUnit, 1)
            write(messageUnit, 45) 
            write(messageUnit, 46) i, data2(j), data3(j)
            write(messageUnit, *)i, ' write attempt'
            go to 900
        endif
     end do
!
    if (cunits(1:3).ne.'CFS') Then
        write(messageUnit, 1)
        write(messageUnit, 61) cunits(1:3)
        go to 910
    endif
!
    if (ctype(1:8).ne.'PER-AVER') Then
        write(messageUnit, 1)
        write(messageUnit, 62) ctype(1:8)
        go to 910
    endif
    end do
!
!

 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Double Expanded Time Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Double Expanded Time Series') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end