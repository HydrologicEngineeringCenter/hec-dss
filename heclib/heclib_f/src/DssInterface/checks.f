     
      subroutine checkString(stringOrig, stringRead, mess, status)
     
      implicit none
     
      integer status
      character*(*) stringOrig, stringRead
      character mess*(*)
      character strOrig*200, stringR*200

        if (stringOrig.ne.stringRead) then
!       Allow different case strings
        strOrig = stringOrig
        stringR = stringRead
        call UPCASE(strOrig)
        call UPCASE(stringR)
        if (strOrig.ne.stringR) then
         write (*,*)' '
         write (*,*)' '
         write (*,*)'***  String read does not match that written *****'
         write (*, 20) stringOrig, stringRead
20       Format('String Written: ==>', A, '<==  Read: ==>',A, '<==')
         write (*,*)mess
         write (*,*)' '
         write (*,*)' '
         status = -1
         return
        endif
        endif
 100  continue

      status = 0
      return
      end
     
     
     
      subroutine checkTimes(dataOrig, dataRead, baseDate,
     * timeGranularityFlag, number, mess, status)
   
      implicit none
   
      integer number, status
      integer dataOrig(number), dataRead(number)
      character mess*(*)
   
      integer i, minutesInBaseDate, baseDate
      integer timeRead, timeGranularityFlag
   
      minutesInBaseDate = baseDate * 1440
      do 100 i=1, number
        if (timeGranularityFlag.eq.1) then
          timeRead = (dataRead(i) / 60) + minutesInBaseDate
        else
          timeRead = dataRead(i) + minutesInBaseDate
        endif
        if (dataOrig(i).ne.timeRead) then
        write (*,*)' '
        write (*,*)' '
        write (*,*)'*****  Data read does not match those written *****'
        write (*, 20) i, dataOrig(i), timeRead
 20     Format('At ordinate: ', I8, ' Written: ', I12, '  Read: ',I12)
         write (*,*)mess
         write (*,*)' '
         write (*,*)' '
        status = -1
        return
        endif
 100  continue

      status = 0
      return
      end


      subroutine checkNotes(dataOrig, dataRead, number, mess, status)
   
      implicit none
   
      integer number, status
      character dataOrig(number)*(*), dataRead(number)*(*)
      character mess*(*)
   
      integer i, n2, n3

      do 100 i=1, number
        call chrlnb(dataOrig(i), n3)
        call chrlnb(dataRead(i), n2)
        if ((n3.le.0).and.(n2.le.0)) go to 100
        if (n3.le.0) n3 = 1
        if (n2.le.0) n2 = 1
        call checkString(dataOrig(i)(1:n3), dataRead(i)(1:n2), mess,
     * status)
        if (status.ne.0) then
            write(*,*)'At ordinate number ',i
            status = -1
            return
        endif
 100  continue

      status = 0
      return
      end
   
      subroutine checkBlanks(dataRead, number, mess, status)
   
      implicit none
   
      integer number, status
      character dataRead(number)*(*)
      character mess*(*)
   
      integer i, n2

      do 100 i=1, number
      call chrlnb(dataRead(i), n2)
      if (n2.ne.0) then
      write (*,*)' '
      write (*,*)' '
      write (*,*)'***** Strings read does not match those written *****'
       write (*,*)mess
       write (*,*)' '
       write (*,*)' '
      write(*,*)'At ordinate number ',i
      status = -1
      return
      endif
 100  continue

      status = 0
      return
      end

