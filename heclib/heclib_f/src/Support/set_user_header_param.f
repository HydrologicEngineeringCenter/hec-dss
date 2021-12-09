      subroutine set_user_header_param(iuhead, kuhead, cparam, 
     *           cvalue, istat)
      !        
      ! Retrieves the value of an associated parameter name from the user header
      !
      ! iuhead  The user header integer array
      ! kuhead  The number of integers in the user header
      ! cparam  The parameter to retrieve the value for
      ! cvalue  The variable to hold the value
      !  
      ! Mike Perryman
      ! USACE Hydrologic Engineering Center
      ! Dec 2021
      !        
      implicit none

      !-------------------!  
      ! define parameters !
      !-------------------!  
      integer   iuhead(*), kuhead, intsRequired, intsAvailable, istat
      character cparam*(*), cvalue*(*)
      !-----------------!  
      ! local variables !
      !-----------------!  
      integer   iuhead_copy(500), max_head_len, ifirst, ilast, i, len
      character cuhead*2000, existing*500
      logical   param_has_separator
      equivalence (iuhead_copy, cuhead)

      include "zdssmz.h"

      !-------------------------------------------------------------------!
      ! copy the user header to a local variable so we can equivalence it !
      !-------------------------------------------------------------------!
      cuhead = " "
      if (kuhead.gt.size(iuhead_copy)) then
        if (mlevel.ge.1) then
          write (munit,"(/a,/,a,i4,/,a,i4)")
     *    " WARNING: Possibly truncating data in user header",
     *    "   User header length : ",kuhead,
     *    "   Truncated length   : ",size(iuhead_copy)
        end if
      end if
      max_head_len = min(kuhead, size(iuhead_copy))
      iuhead_copy(:max_head_len) = iuhead(:max_head_len)
      !--------------------------------------------------------!
      ! blank everything past the copy size or null terminator !
      !--------------------------------------------------------!
      ifirst = index(cuhead, char(0))
      if (ifirst.gt.0) then
        cuhead(ifirst:) = " "
      else
        if (max_head_len.lt.size(iuhead_copy)) then
          cuhead(max_head_len*4:) = " "
        end if
      end if
      !-------------------------------------------------!
      ! see if we have enough room to set the parameter !
      !-------------------------------------------------!
      if (cuhead.eq." ") then
        intsAvailable = kuhead
      else
        intsAvailable = kuhead - (len_trim(cuhead)-1)/4+1
      end if
      call get_user_header_param(iuhead, kuhead, cparam, existing)
      if (existing.eq." ") then
        intsRequired = ((len_trim(cparam)+len_trim(cvalue)+2)-1)/4+1
      else
        intsRequired = ((len_trim(existing)-len_trim(cvalue))-1)/4+1
      end if
      if (intsAvailable.lt.intsRequired) then
        if (mlevel.ge.1) then
          write (munit,"(/a,/,a,i4,/,a,i4)")
     *    " WARNING: Not enough room in user header to set parameter",
     *    "   Length available : ",intsAvailable,
     *    "   Length required  : ",intsRequired
        end if
        istat = 1
      else
        !-------------------------------------------------!
        ! replace the existing parameter or add a new one !
        !-------------------------------------------------!
        if (existing.ne." ") then
          call remove_user_header_param(iuhead_copy, max_head_len, 
     *    cparam)
        end if
        len = len_trim(cuhead)
        if (len.gt.0) then
          if (cuhead(len:len).ne.";") then
            len = len + 1
            cuhead(len:len) = ";"
          end if
        end if
        cuhead(len+1:) = cparam(:len_trim(cparam)) // ":" // 
     *    cvalue(:len_trim(cvalue))
        iuhead(:max_head_len) = iuhead_copy(:max_head_len)
        istat = 0
      end if  
      return

      end subroutine set_user_header_param
