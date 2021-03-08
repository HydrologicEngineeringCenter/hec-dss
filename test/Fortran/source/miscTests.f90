    SUBROUTINE miscTests(ifltab, messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab(*)
      integer number, numb
      character alpha*20
!           !      

      common /lchk/ lcheck
      logical lcheck       
      
      real zmissingFlag   
      external zmissingFlag
!
!
      WRITE(messageUnit, *)'Begin miscTests' 
      status = 0 
      
      call zinqir(ifltab, 'vers', alpha, number)
      if (number.eq.6) then
        call zinquireChar (ifltab, 'vers', alpha, 20, number)
          if (number.ne.6) then
             write(messageUnit, 20)1,number, alpha
             status = -1
             go to 800
          endif
          if (index(alpha(1:2), '6-').ne.1) then
             write(messageUnit, 20)2,number, alpha
             status = -1
             go to 800
          endif
      else if (number.eq.7) then
          call zinquireChar (ifltab, 'vers', alpha, 20, number)
          if (number.ne.7) then
             write(messageUnit, 20)10,number, alpha
             status = -1
             go to 800
          endif
          if (index(alpha(1:2), '7-').ne.1) then
             write(messageUnit, 20)11,number, alpha
             status = -1
             go to 800
          endif
      else
         Write(messageUnit, 20)20,number, alpha
         status = -1
         go to 800
      endif
      call zinquireChar (ifltab, 'nrec', alpha, 20, number)
      if (number.lt.0) then
        write(messageUnit, *)' miscTest fail zinquireChar loc 30, number recs ',number
        status = -2
        go to 800
      endif
      call zinqir(ifltab, 'nrec', alpha, numb)
      if (number.ne.numb) then
        write(messageUnit, *)' miscTest fail zinquireChar loc 31, number recs ',numb
        status = -2
        go to 800
      endif
      
      
 20   format('First miscTests, zinqir call failed.  Location: ', i3, ' number: ',I8, ' alpha: ',A)      
      
    !   call timeTestReadingRTS("s7.dss", "s7paths.txt", messageUnit, status)
    !  return

   ! call zset('mlvl', 'perm', 15)
   ! call zset('mlvl', 'put', 15)

   
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('miscTests Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in miscTests') 
        endif         
       return
!
 900    Continue
        !write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end        