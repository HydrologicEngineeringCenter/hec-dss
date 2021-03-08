    SUBROUTINE test_ITS_CharNotesSameInt(ifltab1, messageUnit, status)
!
!    Test Full writing and reading of irregular interval time series functions
!    
!   There are numerous perpitrations for irregular interval data,
!   so testing is quite complex (way more so than the code), and
!   there is no guarantee that all will be covered.
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(*)
!
      integer itimes1(2200), itimes2(2200), itimes3(2200), itimes4(2200)
    !  Write to DSS array 1, read into array 2, compare with array 3
      !double precision data1(1010), data2(1010), data3(1010)
      double precision data1(2200), data2(2200), data3(2200), data4(2200)   
      integer quality1(2,2200)
      integer quality2(2,2200)
      integer quality3(2,2200)
      integer quality4(2,2200)
      character cnotes1(2200)*50
      character cnotes2(2200)*50
      character cnotes3(2200)*50
      character cnotes4(2200)*50
      character cpart*100
      integer nvals, i, nread 
      integer JULS
      integer j, storeFlag
      logical ascending 
      logical lowerCase
      

      integer k,n,ich
      
      
      character label*2, letter*1

      common /lchk/ lcheck
      logical lcheck
!
!
      WRITE(messageUnit, *)'Begin test_ITS_CharNotesSameInt'
      lowerCase = .false.
      nvals = 1000
      call datjul("06Sep1955", juls, status)
      juls = juls * 1440      
      ich = ichar('A')
      do 20 i=1, nvals
       itimes1(i) = (i * 1440) + juls
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i) 
       data1(i) = i
       data3(i) = data1(i)
       data4(i) = data1(i)
       cnotes1(i) = ' '
       n = mod(i-1,25) + 1
       if (n.eq.1) lowerCase = .not.lowerCase
       if (lowerCase) then
       do 4 k=1,n
          cnotes1(i)(k:k) = char(ich+k-1+32)          
 4     continue
       else
       do 5 k=1,n
          cnotes1(i)(k:k) = char(ich+k-1)          
 5     continue
       endif  
       cnotes3(i) = cnotes1(i)
       cnotes4(i) = cnotes1(i)     
       do 10 j=1,2
        quality1(j,i) = i+j
        quality3(j,i) = quality1(j,i)
 10    continue 
 20  continue
 
 
!     call zset('mlvl', ' ', 12)
!
!   First, write a single value and then read it back
 1  Format('Full Char Notes Irregular Same Interval Len test FAILED')   

    !  write 1000 values and check
    cpart = 'Same Interval'
 

    !   write a data set in the middle of a record (block), then
    !   1.  Write a data set before it
    !   2.  Write a data set after it
    !   3.  Write a data set that exactly encompasses it
    !   4.  Write a data set that starts before and ends in block
    !   5.  Write a data set that starts in the middle and ends after    
    !   6.  Write a data set that starts and ends in the block.
    !   7.  Write a data set that starts before and ends after the block

    !  This is done with 4 perturbations:
    !   a.  Merge, times on (i.e., same interval)
    !   b.  Replace, times on (i.e., same interval)
    !   c.  Merge, times off
    !   d.  Replace, times off


    !for this data, let's use quasi-hourly data (ir-month).  Easier to test within a month block
    nvals = 1000
    call datjul("06Sep1955", juls, status)
    juls = juls * 1440
    do 200 i=1, nvals
       data1(i) = i
       data3(i) = data1(i)
       data4(i) = data1(i)
       itimes1(i) = (i * 60) + juls
       itimes3(i) = itimes1(i) 
       itimes4(i) = itimes1(i)
       quality3(1,i) = quality1(1,i) 
       quality4(1,i) = quality1(1,i)
       quality3(2,i) = quality1(2,i) 
       quality4(2,i) = quality1(2,i)
       cnotes3(i) = cnotes1(i) 
       cnotes4(i) = cnotes1(i) 
 200 continue
 
    ascending = .false.
    do 500 j=1,4
    
        if (j.eq.1) then
            letter = 'a'
            storeFlag = 0
        else if (j.eq.2) then
            letter = 'b'
            storeFlag = 1  
!            call zset('mlvl', '', 10)          
        else if (j.eq.3) then
            letter = 'c'
            storeFlag = 0
        else if (j.eq.4) then
            letter = 'd'
            storeFlag = 1
        endif 
        
        !  If we are at loop 3, we need to use offset times.  
        !  Rebuild the time array by 30 mins off
        if (j.eq.3) then
            do 220 i=1, nvals       
               itimes4(i) = itimes1(i) - 30 
               data4(i) = data1(i) - 0.5             
 220        continue
            !  With the offset, it would be hard to compute all the
            !  results arrays (we'd have to fix gaps), but we know
            !  that the times and data are ascending, so just check that
            !  Almost any error would cause an order issue
            ascending = .true.
        endif
        
        !  label, location, size, totalSize, checkAsending)
  
       !   1.  Write a data set before it
       label = '1' //  letter   
       call test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            quality1, quality2, quality3, quality4, &
            cnotes1, cnotes2, cnotes3, cnotes4, &
            cpart, label, 100, 100, 200, ascending)
        if (status.ne.0) go to 900

        !   2.  Write a data set after it
        label = '2' //  letter
        call test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            quality1, quality2, quality3, quality4, &
            cnotes1, cnotes2, cnotes3, cnotes4, &
            cpart, label, 300, 100, 200, ascending)
        if (status.ne.0) go to 900
        
       !   3.  Write a data set that exactly encompasses it
       nread = 100
       if (j.eq.3) nread = 200
       if (j.eq.4) nread = 101
       label = '3' //  letter
        call test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            quality1, quality2, quality3, quality4, &
            cnotes1, cnotes2, cnotes3, cnotes4, &
            cpart, label, 200, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   4.  Write a data set that starts before and ends in block
         nread = 150
        if (j.eq.3) nread = 200
        if (j.eq.4) nread = 151
        label = '4' //  letter
        call test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            quality1, quality2, quality3, quality4, &
            cnotes1, cnotes2, cnotes3, cnotes4, &
            cpart, label, 150, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   5.  Write a data set that starts in the middle and ends after
        nread = 150
        if (j.eq.3) nread = 200
        if (j.eq.4) nread = 150
        label = '5' //  letter
        call test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            quality1, quality2, quality3, quality4, &
            cnotes1, cnotes2, cnotes3, cnotes4, &
            cpart, label, 250, 100, nread, ascending)
        if (status.ne.0) go to 900
        
        !   6.  Write a data set that starts and ends in the block.
        nread = 100
       if (j.eq.3) nread = 150
       if (j.eq.4) nread = 101
       label = '6' //  letter
        call test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            quality1, quality2, quality3, quality4, &
            cnotes1, cnotes2, cnotes3, cnotes4, &
            cpart, label, 225, 50, nread, ascending)
        if (status.ne.0) go to 900
        
        !   7.  Write a data set that starts before and ends after the block
        nread = 300
       if (j.eq.3) nread = 400
       label = '7' //  letter
        call test_ITS_SupportCharNotes(ifltab1, messageUnit, status, storeFlag, &
            itimes1, itimes2, itimes3, itimes4, &
            data1, data2, data3, data4, &
            quality1, quality2, quality3, quality4, &
            cnotes1, cnotes2, cnotes3, cnotes4, &
            cpart, label, 100, 300, nread, ascending)
        if (status.ne.0) go to 900
        
    
 500  continue   
!
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Full Irregular Interval Time Series, Same Interval Len Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Full Irregular Same Interval Len') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end