     subroutine testOpen(filename1, filename2, messageUnit, &
                          status, ifltab1, ifltab2)
!
!    Test basic opening and closing of new file
!    Internal function
#ifdef _MSC_VER      
       USE IFPORT
       USE IFCORE
#endif

!
      implicit none
      character filename1*(*), filename2*(*)
      integer messageUnit, status, IERR, N
      integer(8) ifltab1(*), ifltab2(*)
      logical logic
!
      character CTEMP*100
!      
     
!
      WRITE(messageUnit, *)'Begin testOpen'
!
!     If files exist, delete them first
      inquire (file=filename1, exist=logic)
      if (logic) then
         call CDELET (filename1,ierr)    
         IF (IERR.NE.0) THEN
#ifdef _MSC_VER           
             IERR = GETLASTERRORQQ()
#endif   
             CTEMP = ' '
             CALL GERROR (CTEMP)
             CALL CHRLNB(CTEMP, N)
             WRITE(messageUnit, 101) filename1
 101	     FORMAT (' Unable to remove old DSS file, name: ', A) 
             WRITE(messageUnit, 102) CTEMP(1:N)
 102	     FORMAT (' Error: ', A)  
            status = ierr       
            return
         else
            Write(messageUnit, 103) filename1
 103        format('File successfully deleted: ', a)
         ENDIF
      endif
!
      inquire (file=filename2, exist=logic)
      if (logic) then
         call CDELET (filename2,ierr)    
         IF (IERR.NE.0) THEN
#ifdef _MSC_VER           
             IERR = GETLASTERRORQQ()
#endif          
             CTEMP = ' '
             CALL GERROR (CTEMP)
             CALL CHRLNB(CTEMP, N)
             WRITE(messageUnit, 101) filename2 
             WRITE(messageUnit, 102) CTEMP(1:N)
            status = ierr       
            return
         else
            Write(messageUnit, 103) filename2
         ENDIF
      endif
!  
!
    call zopen(ifltab1, filename1, status)
    if (status.ne.0) return
    call zclose7(ifltab1)
!
    call zopen7(ifltab2, filename2, status)
    if (status.ne.0) return
    call zclose7(ifltab2)
!
    call zopen7(ifltab1, filename1, status)
    if (status.ne.0) return
    call zerrorCheck(ifltab1, 1, status)
    if (status.ne.0) then
       WRITE(messageUnit, 111) filename1
 111   format('File Error Check failed in file ', a)
       write(messageUnit, 112) status
 112   format('Number of errors detected: ', i8)
       return
    endif
!
    call zopen7(ifltab2, filename2, status)
    if (status.ne.0) return
    call zerrorCheck(ifltab2, 1, status)
    if (status.ne.0) then
       WRITE(messageUnit, 111) filename1
       write(messageUnit, 112) status
       return
    endif
!
!     Looks like files opened successfully
    WRITE(messageUnit, *)'testOpen Passed successfully'
!
    return
    end