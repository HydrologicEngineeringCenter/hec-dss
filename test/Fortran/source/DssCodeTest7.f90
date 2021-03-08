     subroutine DssCodeTest7 (filename1, filename2, messageUnit, status)
!
!
!     DssCodeTest7 is a function to preliminary check the implementation
!     of HEC-DSS Version 7 code.  It is typically used for a port or
!     after checking modifications to the code.
!     It is only capable of basic checks and by no means is exhaustive.
!     Note that this only checks version 7 code; This does not test version 6.
!     
!     Supply the complete names of 2 non-existant DSS file names that
!     can be created for testing purposes.  The DSS files will not be
!     deleted upon completion, but left for inspection, if necessary
!
!     If at any time an error occurs, the process is stopped and a 
!     non-zero status code is returned.  If status zero is returned,
!     then all tests passed.
!
!     Output from this functions is written to unit messageUnit
!
!     This function is written in FORTRAN to test the FORTRAN - C interface functions.
!
!      USE IFPORT
!      USE IFCORE
!
      implicit none
      
      !INCLUDE 'zdssMessagesFort.h'     
!
      character filename1*(*), filename2*(*)
      integer messageUnit, status
      integer highFunction, lowFunction, errorCode, istatus, severity

      common /lchk/ lcheck
      logical lcheck
!
      integer(8) ifltab1(600), ifltab2(600)  
!
!
     write(messageUnit, 10)
10   format('*** Enter DssCodeTest7 ***')
     lcheck = .false.
     !call zset('MLVL', '', 7)
     !call zset('SQUE', 'ON', 1)
     !call zsqueeze('C:/temp/sample76x.dss', ka)
     !call zsqueeze('C:/temp/sample7.dss', ka)
     !return
     
     !call zopen(ifltab1, 'C:/temp/Bulletin_17C_Examples.dss', status);
     !call zset('mlvl', '', 15)
     !call ztsinfo6(ifltab1, '/Santa Cruz River/Lochiel/FLOW-ANNUAL PEAK/01JAN2000/IR-CENTURY/USGS/',ka, kb,ja, jb, cunits, ctype, LQUAL, LDOUBLE, LFOUND) 
     !ztsinfo6 (IFLTAB, CPATH, JULS, ISTIME, JULE, IETIME,
     !* CUNITS, CTYPE, LQUAL, LDOUBLE, LFOUND)
     !call zsetMessageLevel(zmessaging_locking_ID, MESS_INTERNAL_DIAG_2)
     !call zopen(ifltab1, 'C:/temp/TestingPM.dss', status);
     !call testPairedData(ifltab1, messageUnit, status)
     !return
     !call zopen(ifltab2, 'C:/temp/p6a.dss', status);
     !call zcopyFile6(ifltab1, ifltab2, ka)
     !call zcofil6(ifltab1, ifltab2, ia, ka, ib, kb, 0, 0)
     !call test_ITS_Expanions(messageUnit, status)

     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !call zopen(ifltab1, 'C:/temp/SSP/SSP2.1DSS7_Testing/20160226_testing/20160226_SSPwDSS7_testing/20160226_SSPwDSS7_testing.dss', status);
     !call zdatatype7(ifltab1, '/WEST FORK RIVER/NY/FLOW/01Jan1960 - 01Jan1961/1Day/ESTIMATED/', ierr, status, dataType)

     !call zopen(ifltab2, 'C:/Temp/HEC-HMS Testing_old/WLM_MCMC.42/S_Santian_R_CW1.dss', status);
     ! path = '//SS02_S Santiam R/FLOW-BASE/01Feb1996/15Minute/OPT:S Santian R CW1/'
     !number = 1000
     !call zrrtsd(ifltab2, cpath, '10Feb1996', '2400', number, data2, cunits, ctype, IOFSET, status)
     !call zcopyfile(ifltab2, ifltab1, status)
     !call zgetinfo7(ifltab1, '/COLUMBIA RIVER/THE DALLES, OR/FLOW-ANNUAL PEAK/01Jan1900/IR-Century/USGS/', ibuff, status);
     !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     !open(unit=13, file='C:/RTS/database/Russian.dsx')

     !!call zopen(ifltab2, 'C:/RTS/database/Russian.dss', status);
    ! call zsetfi(ifltab2, 'MULT', 'ON', 2,istat)
     !!!iunit = 13
     !call makedsscatalog ('C:/RTS/database/Russian.dss', ifltab2, 'C=PRECIP-INC F=CDEC', numberFound, iunit)
!
!   Test supporting utility functions (These have to work before anything else can)
  !  call testUtilities( messageUnit, status)
    !call test_ITS_Expanions(messageUnit, status)
!
!

!    Test basic opening and closing of new file     
     call testOpen(filename1, filename2, messageUnit, status, &
                    ifltab1, ifltab2)   
     if (status.ne.0) go to 900
!     call testPairedData(ifltab1, messageUnit, status)
!
!    Test basic write, check and read functions
     call runTests(ifltab1, messageUnit, status)
     if (status.ne.0) go to 900
!
!
!    Test Space Recovery
!     call testReclaim (ifltab1, messageUnit, status)
!     if (status.ne.0) go to 900

          
     call testConversion(messageUnit, status)
     if (status.ne.0) go to 900
     !go to 800
!
!    Test Catalog function
     call testCatalog(ifltab1, messageUnit, status)
     if (status.ne.0) go to 900
!
!    Test Multi (Two) threads
!    call testThreads(ifltab1, messageUnit, status)
!    if (status.ne.0) go to 900
!
!    Test Multi User Access
!    call testMultiUser(ifltab1, messageUnit, status)
!    if (status.ne.0) go to 900
     if (lcheck) then 
        call zcheckFile(ifltab1, status)
       if (status.ne.0) then
            Write(messageUnit, 802)
 802        Format('File Check FAILED in main') 
        endif
     endif
     
     call zclose(ifltab1)
     call zclose(ifltab2)
     if (status.ne.0) return
     
     !call zset('MLVL', ' ', 15)
     !call zsqueeze('C:\temp\s6.dss', status)

     
     !  Now run the tests again, detuning file
     !  This should run very slowly (yuck!) but still work
     call testOpen(filename1, filename2, messageUnit, status, &
                    ifltab1, ifltab2)   
     if (status.ne.0) go to 900
     
     call zsetfi(ifltab1, 'detune', '', 1, status)
!
!    Test basic write, check and read functions
        write(messageUnit,*) ' '
        write(messageUnit,*) 'Test De-Tuned file operations '
        write(messageUnit,*) '(File is purposefully set to behave poorly) '
        write(messageUnit,*) ' '
     call runTests(ifltab1, messageUnit, status)
     if (status.ne.0) go to 900
!
!
!    Test Space Recovery
!        write(messageUnit,*) ' '
!        write(messageUnit,*) 'Test Space Recovery '
!        write(messageUnit,*) 'Please WAIT - This can take a long time!!'
!        write(messageUnit,*) ' '
!     call testReclaim (ifltab1, messageUnit, status)
!     if (status.ne.0) go to 900
!!
!
!    Test Catalog function
     call testCatalog(ifltab1, messageUnit, status)
     if (status.ne.0) go to 900
     
     call zclose(ifltab1)
     call zclose(ifltab2)
     
     !  Now run the tests again, Turning off reclaim space (and don't test reclaim!)
     !  This should run very slowly (yuck!) but still work
     call testOpen(filename1, filename2, messageUnit, status, &
                    ifltab1, ifltab2)   
     if (status.ne.0) go to 900
     
     call zsetfi(ifltab1, 'reclaim', '', 0, status)
!
!    Test basic write, check and read functions
        write(messageUnit,*) ' '
        write(messageUnit,*) 'Test space reclaimation '
        write(messageUnit,*) 'Please WAIT - This can take a long time!!'
        write(messageUnit,*) ' '
     call runTests(ifltab1, messageUnit, status)
     if (status.ne.0) go to 900
     
             
     
!  Try the tests with the file detuned and space reclaimation off
     call zclose(ifltab1)
     call zclose(ifltab2)
     
     !  Now run the tests again, Turning off reclaim space (and don't test reclaim!)
     !  This should run very slowly (yuck!) but still work
     call testOpen(filename1, filename2, messageUnit, status, &
                    ifltab1, ifltab2)   
     if (status.ne.0) go to 900
     
     call zsetfi(ifltab1, 'detune', '', 1, status)
     call zsetfi(ifltab1, 'reclaim', '', 0, status)
!
!    Test basic write, check and read functions
        write(messageUnit,*) ' '
        write(messageUnit,*) 'Test both De-Tuned file operations and space reclaimations'
        write(messageUnit,*) '(File is purposefully set to behave poorly) '
        write(messageUnit,*) 'Please WAIT - This can take a long time!!'
        write(messageUnit,*) ' '
     call runTests(ifltab1, messageUnit, status)
     if (status.ne.0) go to 900
     
     
!
 800    continue
        call zclose(ifltab1)
        call zclose(ifltab2)
        Write(messageUnit,*)' '
        Write(messageUnit,*)' '
        Write(messageUnit,*)'-----------------------------------------------------------'
        Write(messageUnit, 810) 
        Write(messageUnit, 811) filename1
        Write(messageUnit, 811) filename2
 810    Format ('All tests passed successfully!')       
 811    Format( 'File tested: ', a) 
        Write(messageUnit,*)'-----------------------------------------------------------'   
         Write(messageUnit,*)' '    
        return
!
!       Error return
 900    Continue
        Write(messageUnit, 820) status
        Write(messageUnit, 811) filename1
        Write(messageUnit, 811) filename2
 820    Format ('DssCodeTest7 FAILED; error: ', I8)
        if (status .gt. 1000) then
            call zerrordecode(status, highFunction, lowFunction, errorCode, istatus, severity)
            write (*,*) 'highFunction   ',highFunction
            write (*,*) 'lowFunction   ',lowFunction
            write (*,*) 'errorCode   ',errorCode
            write (*,*) 'istatus   ',istatus
            write (*,*) 'severity   ',severity
        endif
      return
      end
