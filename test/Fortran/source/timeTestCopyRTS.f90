    SUBROUTINE timeTestCopyRTS(ifltabFrom, ifltabTo, pathsFileName, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltabFrom(600), ifltabTo(600)
      character pathsFileName*(*)
!
      real data1(1010)
      character cpath1*200
      character cunits*12, ctype*12, ctemp*2, dpart*12
      integer npath, count
      integer nvals, i, iofset, mills 


!
  !    WRITE(messageUnit, *)'Begin timeTestCopyRTS'
     

      open (unit=10, file=pathsFileName)
      !call zset('mlvl', ' ', 1)

      mills = 0
     ! call gettimediff (0, mills)

      count = 0
      do 500 i=1,1000000
        read (10, 10, end=600) cpath1
 10     format(a)
        call chrlnb(cpath1, npath)
        count = count + 1
!
    
        NVALS = 1010
        call zrrts(ifltabFrom, cpath1(1:npath), '', '', nvals, data1, cunits, ctype, IOFSET, status)
        if (status.lt.0) go to 900
        call zinqir(ifltabFrom, 'error', ctemp, status)
        if (status.ne.0) go to 900

        call zpathnameGetPart (cpath1(1:npath), 4, dpart)
         call zsrts (ifltabTo, cpath1(1:npath), dpart, '0100', nvals, data1, cunits, ctype, 0, status)
        if (status.ne.0) go to 900
        call zinqir(ifltabTo, 'error', ctemp, status)
        if (status.ne.0) go to 900

 500    continue

 600    continue

         !call gettimediff (1, mills)
         write (messageUnit, *)'======================================'
         write (messageUnit, *)'Execution Time in mills: ',mills
         write (messageUnit, *)'Number of records read: ',count
         write (messageUnit, *)'======================================'
         close(10)

   
!
!
 
       return
!
 900    Continue
        write(messageUnit, 820)
 820        Format('timeTestCopyRTS FAILED ') 
        if (status.eq.0) status = -1
        return
        end