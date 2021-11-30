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

      include 'zdssmz.h'

      cvalue = ' '
      !-------------------------------------------------------------------!  
      ! copy the user header to a local variable so we can equivalence it !
      !-------------------------------------------------------------------!  
      iuhead_copy = 0
      if (nuhead.gt.size(iuhead_copy)) then
        if (mlevel.ge.1) then
          write (munit,'(/a,/,a,a,/,a,i4,/,a,i4)')
     *    ' WARNING: LOCATING PARAMETER VALUE IN TRUNCATED USER HEADER',
     *    '   User header length : ',nuhead,
     *    '   Truncated length   : ',size(iuhead_copy)
        end if
      end if
      max_head_len = min(nuhead, size(iuhead_copy))
      iuhead_copy(:max_head_len) = iuhead(:max_head_len)
      
      !--------------------------------------------------------!
      ! blank everything past the copy size or null terminator !
      !--------------------------------------------------------!
      ifirst = index(cuhead, char(0))
      if (ifirst.gt.0) then
        cuhead(ifirst:) = ' '
      else
        if (max_head_len.lt.size(iuhead_copy)) then
          ifirst = max_head_len*4
          cuhead(ifirst:) = ' '
        end if
      end if
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
          ilast = len_trim(cuhead)
        else
          if (ilast.gt.ifirst + len(cvalue)) then
            ilast = ifirst + len(cvalue) - 1
          else
            ilast = ilast + ifirst - 2
          end if
        end if  
        if (ilast-ifirst+1.gt.len(cvalue)) then
          if (mlevel.ge.1) then
            write (munit,'(/a,/,a,a,/,a,i4,/,a,i4)')
     *      ' WARNING: USER HEADER PARAMETER VALUE TRUNCATED',
     *      '   Parameter       : ',cparam,
     *      '   Value length    : ',ilast-ifirst+1,
     *      '   Variable length : ',len(cvalue)
          end if
        end if;  
        cvalue = cuhead(ifirst:ilast)
      end if
      return
      end

