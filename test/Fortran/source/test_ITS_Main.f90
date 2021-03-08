     subroutine test_ITS_Main (ifltab1, messageUnit, status)
!
!

!
      implicit none
!
      integer messageUnit, status
!
!
      integer(8) ifltab1(600)  

      common /lchk/ lcheck
      logical lcheck
      
     ! call zset('mlvl', ' ', 15)


!   The regular interval tests are comprehensive for compression and uncompression
!   there is no need to repeat the same test for irregular interval
!   What we do need to test for is doubles, quality and notes
!   Qual tests the basic aspects of irregular-interval data.
     call test_ITS_Qual(ifltab1, messageUnit, status)
     if (status.ne.0) go to 100


!   Test automatic expanion of irregular data
    call test_ITS_Expanions( messageUnit, status)
     if (status.ne.0)  go to 100

!
!    Test irregular interval time series write and read     
!    The full test, by itself, is pretty comprehensive
     call test_ITS_Full(ifltab1, messageUnit, status)
     if (status.ne.0)  go to 100


!    Basic test of character notes with irregular-interval data.
     call test_ITS_CharNotes(ifltab1, messageUnit, status)
     if (status.ne.0)  go to 100

     
!    Basic test of character notes with irregular-interval data.
     call test_ITS_CharNotesFull(ifltab1, messageUnit, status)
     if (status.ne.0)  go to 100
     
     !  Test various writing with intervals about the same
     call test_ITS_CharNotesSameInt(ifltab1, messageUnit, status)
     if (status.ne.0)  go to 100

    !  Test various writing with intervals smaller
     call test_ITS_CharNotesSmallerInt(ifltab1, messageUnit, status)
     if (status.ne.0)  go to 100

    !  Test various writing with intervals larger
     call test_ITS_CharNotesLargerInt(ifltab1, messageUnit, status)
     if (status.ne.0)  go to 100


    if (lcheck) call zcheckFile(ifltab1, status)
 100    continue
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('test_ITS_Main Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED test_ITS_Main') 
        endif  
!   
      return
      end
