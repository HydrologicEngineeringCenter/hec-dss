    SUBROUTINE test()
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(1000)
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
      WRITE(messageUnit, *)'Begin test'
      nvals = 1000
      maxVals = 1000
      do i=1, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)  
      end do
  !  goto 210
      
      call zopen6(ifltab1, "test.dss", status)
!
 1  Format('Basic Time Series test FAILED')   
    cpath1 = '/Basic Time Series/One Value/Flow/date/1Hour/F/'
    nvals = 1
    call zsrts (ifltab1, cpath1, '05JUN1957', '2400', nvals, data1(2), 'CFS', 'PER-AVER', 0, status)
     write(messageUnit,*)'zsrts status = ', status
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
!   Check

    IOFSET = 0
    NVALS = 3
    call zrrts(ifltab1, cpath1, '05JUN1957', '2300', NVALS, data2, cunits, ctype, IOFSET, status)
    write(messageUnit,*)'zrrts status = ', status
    call zclose(ifltab1)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
 
!
 800   Continue
       
       return
!
 900    Continue
!        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
    end
    
