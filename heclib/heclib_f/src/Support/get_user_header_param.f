      subroutine get_user_header_param(iuhead, nuhead, cparam, cvalue)
      !        
      ! Retrieves the value of an associated parameter name from the user header
      !
      ! iuhead  The user header integer array
      ! nuhead  The number of integers in the user header
      ! cparam  The parameter to retrieve the value for
      ! cvalue  The variable to hold the value
      !  
      ! Mike Perryman
      ! USACE Hydrologic Engineering Center
      ! Nov 2021
      !        
      implicit none
      !-------------------!  
      ! define parameters !
      !-------------------!  
      integer   iuhead(*), nuhead
      character cparam*(*), cvalue*(*)
      !-----------------!  
      ! local variables !
      !-----------------!  
      integer   iuhead_copy(250), max_head_len, ifirst, ilast, i, plen
      character cuhead*1000
      logical   param_has_separator
      equivalence (iuhead_copy, cuhead)

      cvalue = ' '
      !-------------------------------------------------------------------!  
      ! copy the user header to a local variable so we can equivalence it !
      !-------------------------------------------------------------------!  
      max_head_len = min(nuhead, size(iuhead_copy))
      do i = 1, max_head_len
        iuhead_copy(i) = iuhead(i)
      end do
      !-------------------------------------------------!
      ! blank any null terminator (and following chars) !
      !-------------------------------------------------!
      ifirst = index(cuhead, char(0))
      if (ifirst.gt.0) cuhead(ifirst:) = ' '
      !-----------------------------------------!
      ! locate the parameter name in the header !
      !-----------------------------------------!
      plen = len_trim(cparam)
      param_has_separator = cparam(plen:plen).eq.':'
      if (param_has_separator) then
        ifirst = index(cuhead, cparam(1:plen))
      else  
        ifirst = index(cuhead, cparam(1:plen)//':')
      end if
      !--------------------------------------------------------!
      ! get the parameter value if we found the parameter name !
      !--------------------------------------------------------!
      if (ifirst.gt.0) then
        ifirst = ifirst + plen
        if (.not.param_has_separator) ifirst = ifirst + 1
        ilast = index(cuhead(ifirst:), ';')
        if (ilast.eq.0) then
          ilast = ifirst + len(cvalue)
          cvalue = cuhead(ifirst:)
        else
            if (ilast.gt.ifirst + len(cvalue)) then
              ilast = ifirst + len(cvalue)
            end if  
        end if  
        cvalue = cuhead(ifirst:ilast-1)
      end if
      return
      end

