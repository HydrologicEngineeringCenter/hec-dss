    SUBROUTINE testWrite(ifltab1, messageUnit, status)
!
!    Test basic writing and reading functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(600)
!
      real data1(1000), data2(1000)
      integer userHeader(1000)
      character cpath1*100, cpath2*100
      character cunits*12, ctype*12
      character ctemp*10
      integer istat, ierr 
      integer i, npath1, nuserHeader, nvals
      logical lfound

      common /lchk/ lcheck
      logical lcheck
!
      integer iplan, jplan
!     IPLAN = 0   Always write
!     IPLAN = 1   Only write if new record
!     IPLAN = 2   Only write if old record
!
!     JPLAN = 0:  Use limits NHEAD and NDATA, but do not update NHEAD,
!                 and NDATA
!     JPLAN = 1:  Use limits, but update NHEAD and NDATA
!     JPLAN = 2:  Same as 1, but do not write a warning message if data
!                 is not found
!
!
      WRITE(messageUnit, *)'Begin testWrite'
1     Format('Basic zwrite test FAILED')
!
!     This is a fundemental check on the write/check/read capability of DSS
      cpath1 = '/Test Write/1/c/d/e/f/'
      call chrlnb(cpath1, npath1)
      !  Simple zwrite check
      nvals = 1000
      do 20 i=1,nvals
        data1(i) = float(i)
 20   continue

    !call zset('mlvl', ' ', 17)
    
      iplan = 0
      call zwritea(ifltab1, cpath1, npath1, userHeader, 0, data1, nvals, iplan, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
!
!     Now check that the record is there
      call zcheck(ifltab1, cpath1, npath1, nuserHeader, nvals, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
      if (.not.lfound) Then
        write(messageUnit, 1)
        write(messageUnit, 100) cpath1
 100    format('First record written in DB NOT found: ', A)
        go to 910
     endif
     if (nuserHeader .ne. 0) Then
        write(messageUnit, 1)
        write(messageUnit, 101) nuserHeader
 101    format('Invalid length for User Header: ', i8)
        go to 910
     endif  
     if (nvals .ne.1000) Then
        write(messageUnit, 1)
        write(messageUnit, 102) nvals
 102    format('Invalid length for Data Area: ', i8)
        go to 910
     endif  
!
!
!   Now read the record and verify that the values are correct
      jplan = 1
      nvals = 1000
      call zreada(ifltab1, cpath1, npath1, userHeader, nuserHeader, data2, nvals, jplan, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
      if (.not.lfound) Then
        write(messageUnit, 1)
        write(messageUnit, 200) cpath1
 200    format('First record written in DB NOT found: ', A)
        go to 910
     endif 
     if (nuserHeader .ne. 0) Then
        write(messageUnit, 1)
        write(messageUnit, 201) nuserHeader
 201    format('Invalid length for User Header: ', i8)
        go to 910
     endif  
     if (nvals .ne.1000) Then
        write(messageUnit, 1)
        write(messageUnit, 202) nvals
 202    format('Invalid length for Data Area: ', i8)
        go to 910
     endif 
!
!     Check data values
      do 220 i=1,nvals         
        if (data2(i).ne.data1(i)) then
            write(messageUnit, 1)
            write(messageUnit, 245) 
            write(messageUnit, 246) i, data1(i), data2(i)
 245         Format('Values read do not match those written')
 246         Format('At ordinate: ', I8, ' Values: ', 2F8.2)
            go to 900
        endif
 220  Continue 
 
!
!
    !call zset('mlvl', ' ', 17)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !!!!!  Tests where the hash codes are hardwired
    !!!  To use, set the testing code on in zhash and
    !! boolDoubleCheckPath in zcheck and zupdateBin
    !!  
!    call writeARec(ifltab1, '/a/b/c/d/e/f/', 50, .true., status)
!    call writeARec(ifltab1, '/a/b/c/d/e/f/', 100, .true., status)
!    call writeARec(ifltab1, '/a/b/c/d/e/f/', 200, .true., status)
    !call writeARec(ifltab1, '/g/h/i/j/k/l/', 1000, .true., status)
!    do 30 i=1,800
!    write (cpath1, 29) i
29  format('/a/b/c/d/e/', i3.3, '/')
!    call chrlnb(cpath1, npath1)
!    call writeARec(ifltab1, cpath1(1:npath1), i, .false., status)
!    if (status.ne.0) then
!        write (*,*)'ISSSUES!'
!        return
!     endif    
 30 continue  
!    if (lcheck) call zcheckFile(ifltab1, status) 
!    call zclose(ifltab1) 
!    call writeARec(ifltab1, '/a/b/c/d/e/f/', 50, .true., status)
!    call writeARec(ifltab1, '/a/b/c/d/e/f/', 100, .true., status)
!    call writeARec(ifltab1, '/a/b/c/d/e/f/', 200, .true., status)
    !call writeARec(ifltab1, '/g/h/i/j/k/l/', 1000, .true., status)
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!         
!
!
 800   Continue
       if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Basic zwrite Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in zwrite') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end