    SUBROUTINE testUtilities( messageUnit, status)
!
!    Test utilitiy functions
!           
      implicit none
!      
      integer messageUnit, status
      integer isecs

      integer timeStringToSeconds

	character ctemp*12
   
!
      WRITE(messageUnit, *)'Begin testUtilities'
1     Format('Utilities test FAILED')
!
	isecs = timeStringToSeconds("2400");
	call checkNumbers(86400, isecs, 'testUtilities timeStringToSeconds Seconds wrong, loc 10', status)
	if (status.ne.0) go to 900
	call secondsToTimeString(isecs, 0, 2, ctemp)
	call checkString('24:00:00', ctemp, 'testUtilities secondsToTime Time wrong, loc 10', status)
       if (status.ne.0) go to 900

       isecs = timeStringToSeconds("12:34:56");
	call checkNumbers(45296, isecs, 'testUtilities timeStringToSeconds Seconds wrong, loc 20', status)
	if (status.ne.0) go to 900
	call secondsToTimeString(isecs, 0, 2, ctemp)
	call checkString("12:34:56", ctemp, 'testUtilities secondsToTime Time wrong, loc 20', status)
       if (status.ne.0) go to 900

       isecs = timeStringToSeconds("830");
	call checkNumbers(30600, isecs, 'testUtilities timeStringToSeconds Seconds wrong, loc 30', status)
	if (status.ne.0) go to 900
	call secondsToTimeString(isecs, 0, 2, ctemp)
	call checkString("08:30:00", ctemp, 'testUtilities secondsToTime Time wrong, loc 30', status)
       if (status.ne.0) go to 900

!
 800   Continue      
       Write(messageUnit, 810)
 810   Format('Utilities Passed Successfully')  
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end