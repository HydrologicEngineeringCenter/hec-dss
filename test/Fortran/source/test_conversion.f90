     subroutine testConversion (messageUnit, status)
!
!
!      USE IFPORT
!      USE IFCORE
!
      implicit none
!
      
      integer messageUnit, status, ierr
      integer highFunction, lowFunction, errorCode, istatus, severity
       character c*5

      common /lchk/ lcheck
      logical lcheck
!
!
      integer nrec
      integer ifltabSize
      integer(8) ifltab1(600), ifltab2(600)        
      ifltabSize = 600   
!
!
     write(messageUnit, 10)
 10  format('*** Enter testConversion ***')
     lcheck = .true.
 !   Set the output message level to moderate
    ! call zset('mlvl', ' ', 4)
   !  call zopen(ifltab1, 'C:/temp/sample6.dss', status)
   !  ierr = DELFILESQQ ('C:/temp/sample7convert.dss')
   !  call zopen7(ifltab2, 'C:/temp/sample7convert.dss', status)

     !call zset('mlvl', ' ', 2)
     call zgetfileversion('sample.dss', status)
     if (status.ne.6) then
            write(messageUnit,*)' '
            write(messageUnit,*)'sample.dss is required for conversion test '
            write(messageUnit,*)'It must be the sample.dss file from DSSVue '
            write(messageUnit,*)'And must be a version 6 file'
            if (status.eq.0) then
                write(messageUnit,*)'That file does not exist'
            else if (status.eq.7) then
                write(messageUnit,*)'That file is a version 7 file'
            endif
            write(messageUnit,*)' '
            return 
     endif
     
     call zopen6(ifltab1, 'sample.dss', status)
     if (status.ne.0) return 
     c = ' '
     call zinqir(ifltab1, 'NREC', c, nrec)
     if (nrec.eq.0) then
        write(messageUnit,*)' '
        write(messageUnit,*)' First file for converion test is not correct.'
        write(messageUnit,*)' It should be the sample dss 6 file.'
        status = -1
        return
     endif
     call cdelet ('sample7.dss',ierr)
     call zopen7(ifltab2, 'sample7.dss', status)
     call zcopyfile(ifltab1, ifltab2, status)     
     if (status.eq.123) then
        call zclose(ifltab1)
        call CDELET ('sample6convert.dss',ierr)
        call zopen6(ifltab1, 'sample6convert.dss', status)     
        call zcopyfile(ifltab2, ifltab1, status)
     endif

!
!  
 800    continue
        if (status .ne. 0) go to 900
        call zclose(ifltab1)
        call zclose(ifltab2)
        Write(messageUnit,*)'test conversion  passed successfully!'
  
        return
!
!       Error return
 900    Continue
        Write(messageUnit, 820) status
        !Write(messageUnit, 811) filename1
        !Write(messageUnit, 811) filename2
 811    Format( 'File tested: ', a) 
 820    Format ('test conversion FAILED; error: ', I8)
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
