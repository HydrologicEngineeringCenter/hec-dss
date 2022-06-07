   SUBROUTINE test_RTS_Expansions(messageUnit, status)
!
!    Test writing of time series functions where additional data is
!    written to the record, simulating real-time data
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
!
      double precision data1(1000), data2(1000), data3(1000)
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12, cd*12
      integer nvals, i, iofset, number, kvals 
      integer jul, iyr, imon, iday, min, nd
      integer maxVals
      
      integer iymdjl, jliymd, nexpansions
      real zmissingflag
	  external zmissingflag, iymdjl, jliymd

      common /lchk/ lcheck
      logical lcheck
	  
	  integer ifltabSize
      integer(8) ifltab1(600), ifltab2(600)        
      ifltabSize = 600
      maxVals = 1000
!
!
!     Test Expansion
!
      WRITE(messageUnit, *)'Begin test_RTS_Expansions'

      do i=1,ifltabSize
         ifltab1(i) = 0
         ifltab2(i) = 0
      end do
      
!
  1  Format('Expanded Regular Time Series test FAILED')
!  
      nvals = 1000
      do i=1, nvals
       data1(i) = FLOAT(i)
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
 
    nvals = 0
    cpath1 = '/Expanded Regular Time Series/1/Flow//1Hour/F/'
    
    !  We need new files to check this
    call testOpen('c7t3.dss', 'c7t4.dss',  messageUnit, status, &
                    ifltab1, ifltab2)   
                    
!   Use the beginning of the current month for this test
    call curtim(jul, min)
    i = jliymd(jul, iyr, imon, iday)
    iday = 1
    jul = iymdjl (iyr, imon, iday)
    cd = ' '
    call juldat(jul, 104, cd, nd) 
    
    
    do i=1,20
    
        nvals = nvals + 50
        
        !call zsrts (ifltab1, cpath1, '25JUN1957', '2400', nvals, data1, 'CFS', 'PER-AVER', 0, status)
        call zsrtsd (ifltab1, cpath1, cd, '0100', nvals, data1, 'CFS', 'PER-AVER', 0, status)
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
        kvals = nvals
        number = kvals
       ! call zrrts(ifltab1, cpath1, '25JUN1957', '2400', number, data2, cunits, ctype, IOFSET, status)
        call zrrtsd(ifltab1, cpath1, cd, '0100', number, data2, cunits, ctype, IOFSET, status)

        if (status.lt.0) go to 900
        call zinqir(ifltab1, 'error', ctemp, status)
        if (status.ne.0) go to 900
        call checkString('CFS', cunits, 'test_RTS_Expansions units, loc 10', status)
        if (status.ne.0) go to 900
        call checkString('PER-AVER', ctype, 'test_RTS_Expansions type, loc 10', status)
        if (status.ne.0) go to 900
        call checkDoubles(data3, data2, nvals, 'test_RTS_Expansions Values, loc 10', status)
        if (status.ne.0) go to 900
     !  write (messageUnit, *)'Expansion ',i
!
!
    end do
 
 !  Now copy the file (same as a squeeze)
 !  The record for the current month should remain in an "expanded" state
 !  The record for the other month should have its expansion set to zero
 !  and no additional allocated space.
 
 701 continue
        status = 0
        write (messageUnit, *)' '
        write (messageUnit, *)'Copying file...'        
        call zcopyfile7(ifltab1, ifltab2, status)
        if (status.ne.0) then
            Write(messageUnit, *)'zcopyfile7 fail in test_RTS_Expansions'
            go to 900
        endif
     
!   Now check that the records were copied correctly  
        IOFSET = 0
        kvals = nvals
       ! call zrrts(ifltab1, cpath1, '25JUN1957', '2400', number, data2, cunits, ctype, IOFSET, status)
        call zrrtsd(ifltab2, cpath1, cd, '0100', number, data2, cunits, ctype, IOFSET, status)

        if (status.lt.0) go to 900
        call zinqir(ifltab1, 'error', ctemp, status)
        if (status.ne.0) go to 900
        call checkString('CFS', cunits, 'test_RTS_Expansions units, loc 20', status)
        if (status.ne.0) go to 900
        call checkString('PER-AVER', ctype, 'test_RTS_Expansions type, loc 20', status)
        if (status.ne.0) go to 900
        call checkDoubles(data3, data2, nvals, 'test_RTS_Expansions Values, loc 20', status)
        if (status.ne.0) go to 900
        
!       The number of expansions in the first file should be 5
!       The number in the second file should be 3
!       Note - these might vary according to the number of days in the current month 
        call zinqir7(ifltab1, 'expa', cd, nexpansions)
        if (nexpansions.ne.5) then
            write (messageUnit, *)'Number of expansions is incorrect in expansion test.'
            write (messageUnit, *)'First file, number should be 5;  number is ',nexpansions
            status = -1
        endif
        call zinqir7(ifltab2, 'expa', cd, nexpansions)
        if (nexpansions.ne.3) then
            write (messageUnit, *)'Number of expansions is incorrect in expansion test.'
            write (messageUnit, *)'Second file, number should be 3;  number is ',nexpansions
            status = -1
        endif
        if (status.ne.0) go to 900
               

 800   Continue
        if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Regular Expanded Time Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Regular Expanded Time Series') 
        endif
        
 850   continue
        call zclose(ifltab1)
        call zclose(ifltab2)                 
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        go to 850
        end