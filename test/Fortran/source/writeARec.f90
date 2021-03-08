    SUBROUTINE writeARec(ifltab1, pathname, number, lcheck, status)
!
!    Function to write a record to the file for use in test functions
!   Nothing special, just data and header
!           
      implicit none
!      
      integer messageUnit, number, status
      integer(8) ifltab1(*)
      character pathname*(*)
!
      real data1(1000)
      integer userHeader(1000)
      character ctemp*12
           
      integer numberStored 
      integer i, npath, nuserHeader, nvals
      logical lfound, lcheck
!
      integer iplan
!     IPLAN = 0   Always write
!     IPLAN = 1   Only write if new record
!     IPLAN = 2   Only write if old record
!
!     JPLAN = 0:  Use limits NHEAD and NDATA, but do not update NHEAD,
!                 and NDATA
!     JPLAN = 1:  Use limits, but update NHEAD and NDATA
!     JPLAN = 2:  Same as 1, but do not write a warning message if data
!                 is not found
!
!
1     Format('writeARec FAILED')
!
       messageUnit =5
      call chrlnb(pathname, npath)
      !  Simple zwrite check      
      nvals = number
      if (nvals > 1000) nvals = 1000
      do 20 i=1,nvals
        data1(i) = float(i)
        userHeader(i) = i
 20   continue

      iplan = 0
      call zwritea(ifltab1, pathname, npath, userHeader, nvals, data1, nvals, iplan, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
      if (.not.lcheck) go to 800
!
!     Now check that the record is there
      numberStored = nvals
      call zcheck(ifltab1, pathname, npath, nuserHeader, nvals, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
      if (.not.lfound) Then
        write(messageUnit, 1)
        write(messageUnit, 100) pathname
 100    format('First record written in DB NOT found: ', A)
        go to 910
     endif
     call checkNumbers(nuserHeader, numberStored, 'writeARec user header stored', status)
     if (status.ne.0) go to 900
     call checkNumbers(nvals, numberStored, 'writeARec number stored', status)
     if (status.ne.0) go to 900
!
 800   Continue          
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end