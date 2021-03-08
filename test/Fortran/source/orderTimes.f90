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
      do 5 i=1,nstored
        storTimes(i) = storedTimes(i)
 5    continue
      !  And a copy of what we are writing
      do 10 i=1,nwriting
        writTimes(i) = writingTimes(i)
 10   continue              
      
      
      if (storeFlag.eq.0) then   !  merge
        norder = 0
        do 100 j=1,total
            min = 100000000
           ipos = -1
            do 20 i=1,nstored
                if (storTimes(i).le.min) then
                    min = storTimes(i)
                    ipos = i;
                 endif
 20         continue 
            do 40 i=1,nwriting
                if (writTimes(i).le.min) then
                    min = writTimes(i)
                    ipos = i + nstored;
                 endif
 40         continue  
            if (ipos.eq.-1) go to 800
            norder = norder + 1
            orderedIndex(norder) = ipos
            do 60 i=1,nstored
                if (storTimes(i).le.min) then
                    storTimes(i) =  100000001
                 endif
 60         continue 
            do 80 i=1,nwriting
                if (writTimes(i).le.min) then
                    writTimes(i) =  100000001
                 endif
 80         continue 
 100    continue
 
      else  
         !  Replace
         !  Get times in record already that are before those being written
         norder = 0
         !  Do the times to be written begin before those stored?
         do 120 i=1,nwriting
            if (writTimes(i).ge.storTimes(1)) then
                go to 130
            else
                norder = norder + 1
                orderedIndex(norder) = i + nstored
            endif
 120     continue
 !       To get here, all times being written must be before any stored.
 !       Just add on the stored times
         do 125 i=1,nstored
             norder = norder + 1
             orderedIndex(norder) = i
 125     continue
         ! all done for all written before stored
         go to 800

 130    continue
         !  Determine where the end of writing times is relative to the stored times
        do 160 i=1,nstored
            if (storTimes(i).gt.writTimes(nwriting)) then
                ipos = i
                go to 200
            endif
 160    continue
        !  End of times being written is after times stored.
        ipos = 0

 200    continue
        !  Now we know the relative start and end of writing times (to stored times)        
        if (norder.gt.0) then
            !  Case where first write time starts before first store time
            do 210 i=norder, nwriting
                orderedIndex(i) = i + nstored
 210        continue
            norder = nwriting
        else  ! (norder.eq.0)
            !   Case where first write time starts at or after first store time
            !   Use store times from first store to first write
            do 220 i=1,nstored
               if (storTimes(i).lt.writTimes(1)) then
                   norder = norder + 1
                   orderedIndex(norder) = i
               else
                   go to 230   ! break
               endif
220         continue
230         continue
            do 250 i=1,nwriting
                norder = norder + 1
                orderedIndex(norder) = i + nstored
 250        continue
        endif
        if (ipos.eq.0) then
            !  Case where last write time ends after last store time
        else
        !   Case where last write time ends in store block
            do 300 i=ipos,nstored
                norder = norder + 1
                orderedIndex(norder) = i
 300        continue
        endif
    endif


      
      
 800   continue     
        return
        end