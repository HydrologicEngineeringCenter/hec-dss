    SUBROUTINE timeTestReadingRTS(dssFileName, pathsFileName, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(600)
      character dssFileName*(*), pathsFileName*(*)
!
      real data1(1010)
      character cpath1*200
      character cunits*12, ctype*12, ctemp*12
      integer npath, count
      integer nvals, i, iofset, mills 

      common /lchk/ lcheck
      logical lcheck


!
      WRITE(messageUnit, *)'Reading file ', dssFileName

      call zopen(ifltab1, dssFileName, status)
      if (status.ne.0) go to 900

      open (unit=10, file=pathsFileName)
      call zset('mlvl', ' ', 1)

      mills = 0
     ! call gettimediff (0, mills)

      count = 0
      do i=1,1000000
        read (10, 10, end=600) cpath1
 10     format(a)
        call chrlnb(cpath1, npath)
        count = count + 1
!
    IOFSET = 0
    NVALS = 1010
    call zrrts(ifltab1, cpath1(1:npath), '', '', NVALS, data1, cunits, ctype, IOFSET, status)

    if (status.lt.0) go to 900
    call zinqir(ifltab1, 'error', ctemp, status)
    if (status.ne.0) go to 900

     end do

 600    continue

       !  call gettimediff (1, mills)
         write (messageUnit, *)'Execution Time in mills: ',mills
         write (messageUnit, *)'Number of records read: ',count
         write (messageUnit, *)'======================================'

        call zclose(ifltab1)
        close(10)
   
!
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('timeTestReadingRTS Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('timeTestReadingRTS FAILED ') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 820)
 910    Continue
        if (status.eq.0) status = -1
        return
        end