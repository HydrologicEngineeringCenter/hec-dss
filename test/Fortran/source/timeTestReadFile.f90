      SUBROUTINE timeTestReadFile(dssDir, testFileName, messageUnit, status)

       implicit none
!      
      integer messageUnit, status
      integer(8) ifltab(600)
      character testFileName*(*), dssDir*(*)
      character pathsFileName*256
      integer n
      

      call chrlnb(dssDir, n) 
      pathsFileName = dssDir(1:n) // "dssSource.txt"

      !  Don't show any messages
      call zset('mlvl', ' ', 1)

      call zopen(ifltab, dssDir(1:n) // testFileName, status)
      if (status.ne.0) then
        write (*, *)'  ERROR opening ',testFileName
        return
      endif

      write (*,*)'Reading file ',testFileName
      call timeTestRead(ifltab, pathsFileName, messageUnit, status)  
      
      call zset('mlvl', ' ', 3)
      call zclose(ifltab)   

      return 
      end
  