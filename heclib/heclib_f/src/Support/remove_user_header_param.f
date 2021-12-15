      subroutine remove_user_header_param(iuhead, nuhead, cparam)
      !
      ! Removes a parameter and its associated value from the user header
      !
      ! iuhead  The user header integer array
      ! nuhead  The number of integers in the user header
      ! cparam  The parameter to retrieve the value for
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
      character cparam*(*)
      !-----------------!
      ! local variables !
      !-----------------!
      integer   iuhead_copy(500), max_head_len, ifirst, ilast, i, plen
      character cuhead*2000
      logical   param_has_separator
      equivalence (iuhead_copy, cuhead)

      include 'zdssmz.h'

      !-------------------------------------------------------------------!
      ! copy the user header to a local variable so we can equivalence it !
      !-------------------------------------------------------------------!
      iuhead_copy = 0
      if (nuhead.gt.size(iuhead_copy)) then
        if (mlevel.ge.1) then
          write (munit,'(/a,/,a,i4,/,a,i4)')
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
      ifirst = index(cuhead, cparam(1:plen)//':')
      !-----------------------------------------------!
      ! remove the parameter and its associated value !
      !-----------------------------------------------!
      if (ifirst.gt.0) then
        ilast = index(cuhead(ifirst:), ';')
        if (ilast.eq.0) then
          !------------------!
          ! at end of header !
          !------------------!
          cuhead(ifirst:) = ' '
        else
          !-------------------------!
          ! in the middle of header !
          !-------------------------!
          ilast = ilast + ifirst - 1
          cuhead(ifirst:ilast) = ' '
          cuhead(ifirst:) = cuhead(ilast+1:)
        end if
      end if
      !-------------------------------!
      ! remove any leading delimiters !
      !-------------------------------!
      if (cuhead(1:1).eq.';') cuhead = cuhead(2:)
      iuhead(:max_head_len) = iuhead_copy(:max_head_len)
      return
      end

