    SUBROUTINE runTests(ifltab, messageUnit, status)
!
!    Runs the primary tests (e.g., time series and paired data)
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab(*)
      
      write (*,*)' '
      write (*,*)'Start Run Tests'
      
      
     
     !    Test basic write, check and read functions
     call testWrite(ifltab, messageUnit, status)
     if (status.ne.0) go to 900
!
!    Test Rename and Delete functions
   !  call testRenameDelete(ifltab, messageUnit, status)
     if (status.ne.0) go to 900
!
!    Test expansion write
   !  call testWriteExpansion(messageUnit, status)
     if (status.ne.0) go to 900
     
    
!    Test regular time series write and read
     call test_RTS_Main(ifltab, messageUnit, status)    
     if (status.ne.0) go to 900    
!
    
!    Test irregular time series write and read
     call test_ITS_Main(ifltab, messageUnit, status)    
     if (status.ne.0) go to 900

!
!    Test Paired Data write and read
    call testPairedData(ifltab, messageUnit, status)
     if (status.ne.0) go to 900
!     
     call miscTests(ifltab, messageUnit, status)
!
!    Test Aliases
!    call testAliases(ifltab, messageUnit, status)
     if (status.ne.0) go to 900
!
!    Test Many write and reads
!    call testNumerousWrites(ifltab, messageUnit, status)
     if (status.ne.0) go to 900
     
 800   Continue
       call zcheckFile(ifltab, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('runTests Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in runTests') 
        endif 
        
        write (*,*)'End Run Tests'    
        write (*,*)' '    
       return
!
 900    Continue
        !write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end 