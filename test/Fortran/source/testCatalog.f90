    SUBROUTINE testCatalog( messageUnit, status)
!
!    
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab2(600)
      integer iver
      integer statusWanted;
      integer numberFound
      character dssFileName*50
      character catalogFileName*100
      character pathname*100
      character pathnameFound*100
      
      
      dssFileName = 'sample7.dss'
      call zGetFileVersion (dssFileName, iver)
      if (iver.ne.7) then
        write(messageUnit,*)'Error - A valid DSS 7 test file must exist.  Name: ',dssFileName
        status = -1
        return
      endif

      call zopen(ifltab2, dssFileName, status)
      if (status.ne.0) then
        WRITE(messageUnit, *)'Could not open DSS file: ', dssFileName
        return
      endif

      pathname = '//SACRAMENTO/PRECIP-INC/01Jan2008/1Day/OBS/'
      call zTSsrch7 (ifltab2, pathname, 0, pathnameFound, numberFound)

      open(unit=123, file='sample7.txt')
      call zTSsrch7 (ifltab2, pathname, 123, pathnameFound, numberFound)
      close(123)

      call zclose(ifltab2)

!void zcatalogfile7_(_int64 *ifltab, const char *catalogFileName, int *includeDates, 
!					int *statusWanted, int *istat, int lenCatalogFileName)
!
!
      WRITE(messageUnit, *)'Begin testCatalog, catalog file: ', catalogFileName
1     Format('testCatalog FAILED')

      statusWanted = 0
     ! call zcatalogfile7(ifltab1, catalogFileName, .false., statusWanted, status)
!

!
 800   Continue
       !if (lcheck) call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('testCatalog Passed Successfully')  
        else
            Write(messageUnit, 820) status
 820        Format('File Check FAILED in testCatalog, status = ',I5) 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end