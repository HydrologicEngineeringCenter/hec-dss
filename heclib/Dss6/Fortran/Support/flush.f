      subroutine flush(iunit)
#ifdef _MSC_VER
      use IFCORE
      integer iunit
      logical l
      l = COMMITQQ(IUNIT)
#endif
      return
      end

