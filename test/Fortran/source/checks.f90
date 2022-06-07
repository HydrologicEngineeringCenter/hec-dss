    subroutine checkNumbers(numberOrig, numberRead, mess, status)

    implicit none

    integer status
    integer numberOrig, numberRead
    character mess*(*)

        if (numberOrig.ne.numberRead) then
            write (*,*)' '
            write (*,*)' '
            write (*,*)'*****  Number read does not match that written *****'
            write (*, 20) numberOrig, numberRead
 20         Format('Number Written: ', I10, '  Read: ',I10)
            write (*,*)mess
            write (*,*)' '
            write (*,*)' '
            status = -1
            return
        endif
 100 continue

    status = 0
    return
    end


    subroutine checkString(stringOrig, stringRead, mess, status)

    implicit none

    integer status
    character*(*) stringOrig, stringRead
    character mess*(*)
    character strOrig*200, stringR*200

        if (stringOrig.ne.stringRead) then
!           Allow different case strings
            strOrig = stringOrig
            stringR = stringRead
            call UPCASE(strOrig)
            call UPCASE(stringR)
            if (strOrig.ne.stringR) then 
                write (*,*)' '
                write (*,*)' '
                write (*,*)'*****  String read does not match that written *****'
                write (*, 20) stringOrig, stringRead
     20         Format('String Written: ==>', A, '<==  Read: ==>',A, '<==')
                write (*,*)mess
                write (*,*)' '
                write (*,*)' '
                status = -1
                return
            endif
        endif
 100 continue

    status = 0
    return
    end




    subroutine checkFloats(dataOrig, dataRead, number, mess, status)

    implicit none

    integer number, status
    real dataOrig(number), dataRead(number)
    character mess*(*)

    integer i

    do i=1, number
        if (dataOrig(i).ne.dataRead(i)) then
            write (*,*)' '
            write (*,*)' '
            write (*,*)'*****  Data read does not match those written *****'
            write (*, 20) i, dataOrig(i), dataRead(i)
 20         Format('At ordinate: ', I8, ' Written: ', F12.3, '  Read: ',F12.3)
             write (*,*)mess
             write (*,*)' '
             write (*,*)' '
            status = -1
            return
        endif
     end do

    status = 0
    return
    end


    subroutine checkDoubles(dataOrig, dataRead, number, mess, status)

    implicit none

    integer number, status
    double precision dataOrig(number), dataRead(number)
    character mess*(*)

    integer i

    do i=1, number
        if (dataOrig(i).ne.dataRead(i)) then
            write (*,*)' '
            write (*,*)' '
            write (*,*)'*****  Data read does not match those written *****'
            write (*, 20) i, dataOrig(i), dataRead(i)
 20         Format('At ordinate: ', I8, ' Written: ', F12.3, '  Read: ',F12.3)
             write (*,*)mess
             write (*,*)' '
             write (*,*)' '
            status = -1
            return
        endif
     end do

    status = 0
    return
    end


    subroutine checkInts(dataOrig, dataRead, length, number, mess, status)

    implicit none

    integer length, number, status
    integer dataOrig(length, number), dataRead(length, number)
    character mess*(*)

    integer i, j

    do i=1, number
        do j=1, length
        if (dataOrig(j, i).ne.dataRead(j, i)) then
            write (*,*)' '
            write (*,*)' '
            write (*,*)'*****  Data read does not match those written *****'
            write (*, 20) i, dataOrig(j, i), dataRead(j, i)
 20         Format('At ordinate: ', I8, ' Written: ', I12, '  Read: ',I12)
             write (*,*)mess
             write (*,*)' '
             write (*,*)' '
            status = -1
            return
        endif
      end do
    end do

    status = 0
    return
    end
    
    subroutine checkTimes(dataOrig, dataRead, baseDate, timeGranularitySeconds, number, mess, status)

    implicit none

    integer number, status
    integer dataOrig(number), dataRead(number)
    character mess*(*)

    integer i, minutesInBaseDate, baseDate, timeRead, timeGranularitySeconds
    integer timeGran, timeOrig
    
    minutesInBaseDate = baseDate * 1440
    timeGran = timeGranularitySeconds
    if (timeGran < 1) timeGran = 60
    do i=1, number
        timeRead = (dataRead(i) / timeGran) + minutesInBaseDate
        timeOrig = (dataOrig(i) / timeGran) + minutesInBaseDate
        if (timeOrig.ne.timeRead) then
            write (*,*)' '
            write (*,*)' '
            write (*,*)'*****  Data read does not match those written *****'
            write (*, 20) i, dataOrig(i), dataRead(i)
            write (*, *) i, timeOrig, timeRead
 20         Format('At ordinate: ', I8, ' Written: ', I12, '  Read: ',I12)
             write (*,*)mess
             write (*,*)' '
             write (*,*)' '
            status = -1
            return
        endif
     end do

    status = 0
    return
    end
   

    subroutine checkNotes(dataOrig, dataRead, number, mess, status)

    implicit none

    integer number, status
    character dataOrig(number)*(*), dataRead(number)*(*)
    character mess*(*)

    integer i, j, n2, n3

    do i=1, number
        call chrlnb(dataOrig(i), n3)
        call chrlnb(dataRead(i), n2)
        if (.not.((n3.le.0).and.(n2.le.0))) then
            if (n3.le.0) n3 = 1
            if (n2.le.0) n2 = 1
            call checkString(dataOrig(i)(1:n3), dataRead(i)(1:n2), mess, status)
            if (status.ne.0) then
                write(*,*)'At ordinate number ',i
                status = -1
                return
            endif
        end if
    end do

    status = 0
    return
    end
    
    subroutine checkBlanks(dataRead, number, mess, status)

    implicit none

    integer number, status
    character dataRead(number)*(*)
    character mess*(*)

    integer i, j, n2, n3

    do i=1, number
        call chrlnb(dataRead(i), n2)
        if (n2.ne.0) then
            write (*,*)' '
            write (*,*)' '
            write (*,*)'*****  Strings read does not match those written *****'
             write (*,*)mess
             write (*,*)' '
             write (*,*)' '
            write(*,*)'At ordinate number ',i
            status = -1
            return
        endif
    end do

    status = 0
    return
    end
