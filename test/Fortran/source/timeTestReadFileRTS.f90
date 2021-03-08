      SUBROUTINE timeTestReadFileRTS(dssDir, testFileName, messageUnit, status)

       implicit none
!      
      integer messageUnit, status
      character testFileName*(*), dssDir*(*)
      character pathsFileName*256
      integer n
      

      call chrlnb(dssDir, n) 
      pathsFileName = dssDir(1:n) // "dssSource.txt"

      !  Don't show any messages
      call zset('mlvl', ' ', 1)

      call timeTestReadingRTS(dssDir(1:n) // testFileName, pathsFileName, messageUnit, status)      

      return 
      end
  