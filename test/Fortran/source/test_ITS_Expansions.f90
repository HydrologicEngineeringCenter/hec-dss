    SUBROUTINE test_ITS_Expanions(messageUnit, status)
!
!    Test expansion writing of irregular interval time series functions
!    
!      USE IFPORT
!      USE IFCORE
!           
      implicit none
      
       integer messageUnit, status

      integer itimes1(2010), itimes2(2010), itimes3(2010)
      real data1(2010), data2(2010), data3(2010)
      character cpath1*100
      character cunits*12, ctype*12, ctemp*12
      integer ierr
      integer nvals, i, nread
      integer JULS, ISTIME, JULE, IETIME
      integer jul, min, iyr, imon, iday
      integer baseDate, nexpansions
      
      character cd*10
      
      integer jliymd, iymdjl
      
      integer ifltabSize
      integer(8) ifltab1(600), ifltab2(600)

      common /lchk/ lcheck
      logical lcheck
      
              
      ifltabSize = 600
     
!
      WRITE(messageUnit, *)'Begin test_ITS_Expanions'
      
      
          !  We need new files to check this
     call CDELET('c7t5.dss',ierr)
     call CDELET('c7t6.dss',ierr)
     call testOpen('c7t5.dss', 'c7t6.dss',  messageUnit, status, &
                    ifltab1, ifltab2)
      
      if (status.ne.0) then
          write(messageUnit,*)'Unable to open scratch file DssExpansionTest.dss'
          write(messageUnit,*)'Error = ',ierr
          WRITE(messageUnit, *)'failed in test_ITS_Expanions'
          status = -1
          return
      endif
      
      
      nvals = 2000
!   Use the beginning of the current year for this test
    call curtim(jul, min)
    i = jliymd(jul, iyr, imon, iday)
    iday = 2
    imon = 1
    juls = iymdjl (iyr, imon, iday)     
      itimes1(1) = juls * 1440
      itimes3(1) = itimes1(1)
      data1(1) = 1.0
      data3(1) = 1.0
      do 20 i=2, nvals
       data1(i) =FLOAT(i)
       data3(i) = data1(i)
       if (i.lt.500) then
        itimes1(i) = itimes1(i-1) + 720
       else if (i.lt.1000) then
        itimes1(i) = itimes1(i-1) + 360
       else if (i.lt.1500) then
        itimes1(i) = itimes1(i-1) + 180
       else 
        itimes1(i) = itimes1(i-1) + 100
       endif
       itimes3(i) = itimes1(i) 
 20  continue
 
    jule = itimes1(nvals) / 1440 + 5
 
 
   !  call zset('mlvl', ' ', 12)
!
!  
 1  Format('Expansion Irregular Interval Time Series test FAILED')   

    ! 
    cpath1 = '/Irregular Time Series/Expansion/Flow/date/~1Day/F/'
    
    do 500 i=1,40
    
    nvals = i * 50
    baseDate = 0 
    call zsits(ifltab1, cpath1, itimes1, data1, nvals, baseDate, 'CFS', 'PER-AVER', 0, status)
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
    

    istime = 0
    ietime = 0
    baseDate = 0   
    call zrits(ifltab1, cpath1, JULS, ISTIME, JULE, IETIME, itimes2, data2, 2000, nread, baseDate, cunits, ctype, status)
   
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
     call checkNumbers(nvals, nread, 'test_ITS_Expanions number stored, loc 10', status)
    if (status.ne.0) go to 900
    call checkTimes(itimes3, itimes2, baseDate, 60, nread, 'test_ITS_Expanions times, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Expanions units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Expanions type, loc 10', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_ITS_Expanions Values, loc 10', status)
    if (status.ne.0) go to 900
  !  write (messageUnit, *)'Expansion ',i
  
 
        
    
 500  continue  
 
 !  Now copy the file (same as a squeeze)
 !  The record for the current month should remain in an "expanded" state
 !  The record for the other month should have its expansion set to zero
 !  and no additional allocated space.
 
        status = 0
        write (messageUnit, *)' '
        write (messageUnit, *)'Copying file...'        
        call zcopyfile7(ifltab1, ifltab2, status)
        if (status.ne.0) then
             write (messageUnit, *)'Error in squeeze in test_ITS_Expanions: ',status
             return
        endif
        
     
!   Now check that the records were copied correctly 
    istime = 0
    ietime = 0
    baseDate = 0   
    call zrits(ifltab2, cpath1, JULS, ISTIME, JULE, IETIME, itimes2, data2, 2000, nread, baseDate, cunits, ctype, status)
   
    if (status.ne.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900
     call checkNumbers(nvals, nread, 'test_ITS_Expanions number stored, loc 10', status)
    if (status.ne.0) go to 900
     call checkInts(itimes3, itimes2, 1, nread, 'test_ITS_Expanions times, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('CFS', cunits, 'test_ITS_Expanions units, loc 10', status)
    if (status.ne.0) go to 900
    call checkString('PER-AVER', ctype, 'test_ITS_Expanions type, loc 10', status)
    if (status.ne.0) go to 900
    call checkFloats(data3, data2, nvals, 'test_ITS_Expanions Values, loc 10', status)
    if (status.ne.0) go to 900  
!
!
!       The number of expansions in the first file should be 6
!       The number in the second file should be 4
!       Note - these might vary according to the number of days in the current month 
        call zinqir7(ifltab1, 'expa', cd, nexpansions)
        if (nexpansions.ne.6) then
            write (messageUnit, *)'Number of expansions is incorrect in expansion test.'
            write (messageUnit, *)'First file, number should be 8;  number is ',nexpansions
            status = -1
        endif
        call zinqir7(ifltab2, 'expa', cd, nexpansions)
        if (nexpansions.ne.4) then
            write (messageUnit, *)'Number of expansions is incorrect in expansion test.'
            write (messageUnit, *)'Second file, number should be 4;  number is ',nexpansions
            status = -1
        endif
        if (status.ne.0) go to 900
        
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Expansion Irregular Interval Time Series Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Expansion Irregular Interval Time Series') 
        endif 
        call zclose(ifltab1) 
        call zclose(ifltab2)       
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        call zclose(ifltab1) 
        call zclose(ifltab2)
        if (status.eq.0) status = -1
        return
        end