      SUBROUTINE CHRLNB (str,rval)    
      implicit none
      character(len=*), intent(in):: str
      integer, intent(out):: rval
      
      if( len(str) == 0) then 
          rval =1  ! duplcate old code.
      else 
         rval = len_trim(str)
      endif

      return
      end

