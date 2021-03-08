    SUBROUTINE timeTestFileParams(messageUnit, status)
!
!    Test basic writing and reading of time series functions
!    Internal function
!           
      implicit none
!      
      integer messageUnit, status
      character dssDir*80

!
  !    WRITE(messageUnit, *)'Begin timeTestCopy'
      dssDir = ""

      !call timeTestWriteFile(dssDir, "dss0x0.dss", 0, 0, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss4096x100.dss", 4096,100, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss4096c200.dss", 4096,200, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss4096x300.dss", 4096,300, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss4096x500.dss", 4096,500, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss8192x100.dss", 8192,100, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss8192x200.dss", 8192,200, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss8192x300.dss", 8192,300, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss8192x500.dss", 8192,500, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss16348x100.dss", 16348,100, messageUnit, status)
      call timeTestWriteFile(dssDir, "dss16348x200.dss", 16348,200, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss16348x300.dss", 16348,300, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss16348x500.dss", 16348,500, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss32768x100.dss", 32768,100, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss32768x200.dss", 32768,200, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss32768x300.dss", 32768,300, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss32768x500.dss", 32768,500, messageUnit, status)
      !call timeTestWriteFile(dssDir, "dss0x0a.dss", 0, 0, messageUnit, status)

      !call timeTestReadFile(dssDir, "dss0x0.dss", 0, 0, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss4096x100.dss", 4096,100, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss4096c200.dss", 4096,200, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss4096x300.dss", 4096,300, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss4096x500.dss", 4096,500, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss8192x100.dss", 8192,100, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss8192x200.dss", 8192,200, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss8192x300.dss", 8192,300, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss8192x500.dss", 8192,500, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss16348x100.dss", 16348,100, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss16348x200.dss", 16348,200, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss16348x300.dss", 16348,300, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss16348x500.dss", 16348,500, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss32768x100.dss", 32768,100, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss32768x200.dss", 32768,200, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss32768x300.dss", 32768,300, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss32768x500.dss", 32768,500, messageUnit, status)
      !call timeTestReadFile(dssDir, "dss0x0a.dss", 0, 0, messageUnit, status)

     

      !timeTestReadFile
     
 910    Continue
        if (status.eq.0) status = -1
        return
        end

       