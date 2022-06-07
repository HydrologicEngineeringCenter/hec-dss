    SUBROUTINE orderTimes(storedTimes, nstored, writingTimes, nwriting, storeFlag, orderedIndex, norder)
!
!   storeFlag = 0, merge.    = 1, replace
!   storedTimes is what is already in the record
!   writingTimes is what we are writing, and writing over store times
!           
!
!   In the orderedIndex array, values of 1 to nstored indicate time is from storedTimes
!   values > nstored indicate the time is from writingTimes
!   This shows what time array to use for each order position
!

      implicit none
      integer nstored, nwriting, norder, storeFlag
      integer storedTimes(nstored), writingTimes(nwriting), orderedIndex(*)
      integer storTimes(3000), writTimes(3000)   
!      
      integer i, j, min, ipos, total
      
      
      total = nstored + nwriting
      if (total.gt.3000) then
        write(*,*)'Insufficent size in orderTimes'
        norder = -1
        return
      endif
      
      !  Make a copy of the times already in the record
      do i=1,nstored
        storTimes(i) = storedTimes(i)
      end do
      !  And a copy of what we are writing
      do i=1,nwriting
        writTimes(i) = writingTimes(i)
      end do
      
      
      if (storeFlag.eq.0) then   !  merge
        norder = 0
        do j=1,total
            min = 100000000
           ipos = -1
            do i=1,nstored
                if (storTimes(i).le.min) then
                    min = storTimes(i)
                    ipos = i;
                 endif
            end do
            do i=1,nwriting
                if (writTimes(i).le.min) then
                    min = writTimes(i)
                    ipos = i + nstored;
                 endif
            end do
            if (ipos.eq.-1) go to 800
            norder = norder + 1
            orderedIndex(norder) = ipos
            do i=1,nstored
                if (storTimes(i).le.min) then
                    storTimes(i) =  100000001
                 endif
            end do
            do i=1,nwriting
                if (writTimes(i).le.min) then
                    writTimes(i) =  100000001
                 endif
            end do
        end do
 
      else  
         !  Replace
         !  Get times in record already that are before those being written
         norder = 0
         !  Do the times to be written begin before those stored?
         do i=1,nwriting
            if (writTimes(i).ge.storTimes(1)) then
                go to 130
            else
                norder = norder + 1
                orderedIndex(norder) = i + nstored
            endif
         end do
 !       To get here, all times being written must be before any stored.
 !       Just add on the stored times
         do i=1,nstored
             norder = norder + 1
             orderedIndex(norder) = i
         end do
         ! all done for all written before stored
         go to 800

 130    continue
         !  Determine where the end of writing times is relative to the stored times
        do i=1,nstored
            if (storTimes(i).gt.writTimes(nwriting)) then
                ipos = i
                go to 200
            endif
        end do
        !  End of times being written is after times stored.
        ipos = 0

 200    continue
        !  Now we know the relative start and end of writing times (to stored times)        
        if (norder.gt.0) then
            !  Case where first write time starts before first store time
            do i=norder, nwriting
                orderedIndex(i) = i + nstored
            end do
            norder = nwriting
        else  ! (norder.eq.0)
            !   Case where first write time starts at or after first store time
            !   Use store times from first store to first write
            do i=1,nstored
               if (storTimes(i).lt.writTimes(1)) then
                   norder = norder + 1
                   orderedIndex(norder) = i
               else
                   go to 230   ! break
               endif
            end do
230         continue
            do i=1,nwriting
                norder = norder + 1
                orderedIndex(norder) = i + nstored
            end do
        endif
        if (ipos.eq.0) then
            !  Case where last write time ends after last store time
        else
        !   Case where last write time ends in store block
            do i=ipos,nstored
                norder = norder + 1
                orderedIndex(norder) = i
            end do
        endif
    endif


      
      
 800   continue     
        return
        end