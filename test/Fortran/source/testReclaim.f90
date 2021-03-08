    SUBROUTINE testReclaim (ifltab1, messageUnit, status)
!
!    Test recalim functions, both pathname bin and general space
!    This is done by having a big DSS file, deleteing a lot of records
!    and then re-writing the same number (but different) w/ same size
!    The ending file size should be slightly larger than original
!           
      implicit none
!      
      integer messageUnit, status
      integer(8) ifltab1(600)
!
      real data1(1000)
      integer userHeader(1000)
      character cpath1*100
      character ctemp*10
      integer i, npath1, nvals, k
      logical lfound
      integer ibdead, ibnrec, ibfsiz
      integer iedead, ienrec, iefsiz

      common /lchk/ lcheck
      logical lcheck
!
      integer iplan

!
      WRITE(messageUnit, *)'Begin testReclaim'
1     Format('Basic Reclaim test FAILED')

      nvals = 1000
      do 20 i=1,nvals
        data1(i) = float(i)
 20   continue
 
      do 30 k=1,20000
!     Write a bunch of records
      cpath1 = '/Test Write/bbb/cccc/dddd/eeee/zzzzz/'
      write (ctemp, '(I5.5)') k
      cpath1(32:36) = ctemp(1:5)
      call chrlnb(cpath1, npath1)
      
      if (k.eq.10624) then
        write (*,*)'In test reclaim'
      endif

      iplan = 0
      call zwritea(ifltab1, cpath1, npath1, userHeader, 0, data1, nvals, iplan, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
!
 30 continue 
      WRITE(messageUnit, *)'testReclaim, Pass A'
 
      do 40 k=1,5000
!     Now write ones we will delete
      cpath1 = '/More Write/bbbbb/cccc/dddd/eeee/zzzz/'
      write (ctemp, '(I5.5)') k
      cpath1(13:17) = ctemp(1:5)
      call chrlnb(cpath1, npath1)

      iplan = 0
      call zwritea(ifltab1, cpath1, npath1, userHeader, 0, data1, nvals, iplan, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
!
 40 continue 
    WRITE(messageUnit, *)'testReclaim, Pass B'
 
 !  zinqir7_ (_int64 *ifltab, const char *request, char *creturn, int *number,
	!		   int lenRequest, int lencreturn)
      call zinqir7(ifltab1, 'nrec', ctemp, ibnrec)
      call zinqir7(ifltab1, 'fsiz', ctemp, ibfsiz)
      call zinqir7(ifltab1, 'dead', ctemp, ibdead)
 
      do 50 k=1,5000
!     Now delete those
      cpath1 = '/More Write/bbbbb/cccc/dddd/eeee/zzzz/'
      write (ctemp, '(I5.5)') k
      cpath1(13:17) = ctemp(1:5)
      call chrlnb(cpath1, npath1)

      iplan = 0
      call zdelet(ifltab1, cpath1(1:npath1), npath1, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
!
 50 continue
    WRITE(messageUnit, *)'testReclaim, Pass C'
      do 60 k=1,5000
!     Write the same paths.  They have to be the same to have same hash
!     and allow us to test path bin recovery
       cpath1 = '/More Write/bbbbb/cccc/dddd/eeee/zzzz/'
      write (ctemp, '(I5.5)') k
      cpath1(13:17) = ctemp(1:5)
      call chrlnb(cpath1, npath1)

      iplan = 0
      call zwritea(ifltab1, cpath1, npath1, userHeader, 0, data1, nvals, iplan, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
!
 60 continue 
    WRITE(messageUnit, *)'testReclaim, Pass D'
 
      call zinqir7(ifltab1, 'nrec', ctemp, ienrec)
      call zinqir7(ifltab1, 'fsiz', ctemp, iefsiz)
      call zinqir7(ifltab1, 'dead', ctemp, iedead)
      
      do 70 k=1,5000
!     Now write more to be sure file still good
      cpath1 = '/More Reclaim/b/cccc/dddd/eeee/zzzzz/'
      write (ctemp, '(I5.5)') k
      cpath1(32:36) = ctemp(1:5)
      call chrlnb(cpath1, npath1)

      iplan = 0
      call zwritea(ifltab1, cpath1, npath1, userHeader, 0, data1, nvals, iplan, lfound)
      call zinqir(ifltab1, 'error', ctemp, status)
      if (status.ne.0) go to 900
!
 70 continue
        WRITE(messageUnit, *)'testReclaim, Pass E'
 
        write(messageUnit,*)'starting records: ',ibnrec,'  Ending records: ',ienrec
        write(messageUnit,*)'starting file size: ',ibfsiz,'  Ending file size: ',iefsiz
        write(messageUnit,*)'starting dead Space: ',ibdead,'  Ending dead Space: ',iedead
       
!
!
 800   Continue
        write(messageUnit,*) ' '
        write(messageUnit,*) 'Please WAIT - This can take a long time!!'
        write(messageUnit,*) ' '
       call zcheckFile(ifltab1, status)
       if (status.eq.0) then
            Write(messageUnit, 810)
 810        Format('Reclaim Passed Successfully')  
        else
            Write(messageUnit, 820)
 820        Format('File Check FAILED in Reclaim') 
        endif         
       return
!
 900    Continue
        write(messageUnit, 1)
 910    Continue
        if (status.eq.0) status = -1
        return
        end