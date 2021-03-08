    SUBROUTINE test_RTS_SecondIntervals(ifltab1, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
      real data1(100010), data2(100010), data3(100010)
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12
      integer nvals, i, iofset 

      common /lchk/ lcheck
      logical lcheck

      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin test_RTS_Basic'
      nvals = 100000
      do 20 i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)  
 20  continue
 
   ! call zset('mlvl', ' ', 17)
!
 1  Format('Time Series Intervals test FAILED')   
   
    cpath1 = '/Time Series Intervals/1/Flow/date/1Second/F/'
    data3(1) = data1(1)
    data3(3) = data1(3)
    nvals = 100000
    call zsrts (ifltab1, cpath1, '05JUN1957', '12:34:56', nvals, data1, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    IOFSET = 0
    NVALS = 100000
    call zrrts(ifltab1, cpath1, '05JUN1957', '12:34:56', NVALS, data2, cunits, ctype, IOFSET, status)

    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_RTS_Basic units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_RTS_Basic type, loc 10', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_RTS_Basic Values, loc 10', status)
    if (status.ne.0) go to 900


    !  hourly
    cpath1 = '/Time Series Intervals/1/Flow/date/1Hour/F/'
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

!
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Time Series Intervals Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Time Series Intervals') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end