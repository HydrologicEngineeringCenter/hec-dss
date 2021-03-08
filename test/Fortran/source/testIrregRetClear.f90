    SUBROUTINE ztsIrregRetClear(ifltab1, cpath1, cdate1in, ctime1in, cdate2in, ctime2in, kvals, nvals, &
    itimes2, granularity, baseDate, data2, sizeData, lenData, quality2, sizeQuality, lenQuality,  &
    notes2, sizeNotes, lenNotes, cnotes, sizeCnotes, totalNotesRead, userHeader, izero, uhlength, &
    cunits, ctype, coordinates, jzero, clength, &
    icoord, kzero, klength, ctemp, status)

!
!    Clear all variables to prepare for a call to ztsIrregRetrieve
!           
      implicit none
!      
    
      integer(8) ifltab1(*)
      integer kvals, nvals
      integer  sizeData, lenData, sizeQuality, lenQuality, sizeNotes, lenNotes 
      integer sizeCnotes, totalNotesRead, izero, uhlength, jzero, clength
      integer kzero, klength, ctemp, status 
      integer granularity, baseDate

      integer data2(sizeData, kvals)
      integer quality2(sizeQuality, kvals)
      integer notes2(sizeNotes, kvals)
      integer itimes2(kvals)

      character cpath1*(*), cdate1in*(*), ctime1in*(*), cdate2in*(*), ctime2in*(*)
      character cnotes(sizeCnotes)*1, cunits*(*), ctype*(*)
!
      integer i, j
      
!
      integer userHeader(1)
      double precision coordinates(1)
      integer icoord(1)   

      
 
      do 20 i=1, kvals
        itimes2(i) = 0
      do 12 j=1,sizeData
         data2(j, i) = 0
 12   continue
      do 13 j=1,sizeQuality
         quality2(j, i) = 0
 13   continue
      do 14 j=1,sizeNotes
         notes2(j, i) = 0
 14   continue          
 20   continue

      lenData = 0
      lenQuality = 0
      lenNotes = 0

      cunits = ' '
      ctype = ' ' 

      if (sizeCnotes .gt. 0) then
        do 40 i=1,sizeCnotes
           cnotes(i) = ' '
 40     continue
      endif

        status = -1
        return
        end