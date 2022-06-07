    SUBROUTINE timeTestRead(ifltab, pathsFileName, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status, precision
      integer(8) ifltab(600)
      character pathsFileName*(*)
!
      real data1(1010)
      character cpath1*200
      character cunits*12, ctype*12, ctemp*2, dpart*12
      integer ierr, npath, count, n, ndata
      integer nvals, i, iofset, mills 
      integer idum(10)
      integer logicalNumberValues, dataType


!
  !    WRITE(messageUnit, *)'Begin timeTestRead'
     

      open (unit=10, file=pathsFileName)
      !call zset('mlvl', ' ', 1)

      mills = 0
     ! call gettimediff (0, mills)

      count = 0
      do i=1,1000000
        read (10, 10, end=600) cpath1
 10     format(a)
        npath = index(cpath1, '/', .true.)      
        !call chrlnb(cpath1, npath)
        count = count + 1
!
    
      !  call zhash (ifltab, cpath1, npath, n)
      
         call zreadx(ifltab, cpath1(1:npath), idum, 0, n, idum, 0, n, idum, 0, n, idum, 0, n, data1, 1, ndata, &
         logicalNumberValues, dataType, ierr) 
         if (ierr.eq.0) go to 900   
        call zinqir(ifltab, 'error', ctemp, status)
        if (status.ne.0) go to 900


     end do

 600    continue

       !  call gettimediff (1, mills)        
         write (messageUnit, *)'Execution Time in mills: ',mills
         write (messageUnit, *)'Number of records read: ',count
         write (messageUnit, *)'======================================'
         close(10)

   
!
!
 800   Continue      
       return
!
 900    Continue
        write(messageUnit, 820)
 820        Format('timeTestRead FAILED ') 
 910    Continue
        if (status.eq.0) status = -1
        return
        end