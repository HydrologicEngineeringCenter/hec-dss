      SUBROUTINE timeTestWriteFileRTS(dssDir, testFileName, hashSize, binSize, messageUnit, status)

       implicit none
!      
      integer messageUnit, status, hashSize, binSize 
      integer(8) ifltabFrom(600), ifltabTo(600)
      character testFileName*(*), dssDir*(*)
      character source*256
      character pathsFileName*256
      integer n
      integer zopenextended
      external zopenextended
      

      call chrlnb(dssDir, n)
      source = dssDir(1:n) // "dssSource.dss"      
      pathsFileName = dssDir(1:n) // "dssSource.txt"

      !  Don't write anything
      call zset('mlvl', ' ', 1)

      status = zopenextended(ifltabTo, dssDir(1:n) // testFileName, 7, 0, 0, hashSize, binSize)
      if (status.ne.0) then
        write (*, *)'  ERROR opening ',dssDir(1:n) // testFileName
        return
      endif
      
      call zopen(ifltabFrom, source, status)
      if (status.ne.0) then
        write (*, *)'  ERROR opening ',source
        return
      endif

      write(messageUnit,*)'Testing Hash size: ',hashSize,', Bin size: ',binSize
      
      call timeTestCopyRTS(ifltabFrom, ifltabTo, pathsFileName, messageUnit, status)

      call zset('mlvl', ' ', 3)
      call zclose(ifltabFrom)
      call zclose(ifltabTo)

      return 
      end
  