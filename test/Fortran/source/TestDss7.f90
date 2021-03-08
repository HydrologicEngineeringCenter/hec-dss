!  TestDss7.f90 
!
!  FUNCTIONS:
!  TestDss7 - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: TestDss7
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

    program TestDss7

    implicit none

    integer status, messageUnit
    status = 0
    messageUnit = 6
 
 

!    call workbench()
!    call exit()


    call deletefile('c7t1.dss',status)
    call deletefile('c7t2.dss',status)
    call deletefile('c7t3.dss',status)
    call deletefile('c7t4.dss',status)
   call julian0_31dec1899()

    call DssCodeTest7 ('c7t1.dss', 'c7t2.dss', messageUnit, status)
 
    call exit(status)
    
    end program TestDss7
    
    subroutine julian0_31dec1899()

   
   
    end subroutine
    
   subroutine workbench()
   integer(8) ifltab1(600)
   real data1(1010)
   integer i,status, messageUnit, nvals
   character cpath*100
   status =0
   messageUnit = 6
   
   cpath = '/BCLINE/2DArea: Downstream/FLOW/31DEC1899/10MIN/Unsteady/'
   
   call deletefile('workbench.dss' ,status)
   call zopen(ifltab1, 'workbench.dss', status)
    if ( status.ne.0) return

!   call zset('MLEVEL', ' ', 15)
   
   nvals = 1000
   do 20 i=1, nvals
       data1(i) =FLOAT(i) 
 20  continue
    
! fails  call zsrts (ifltab1, cpath, '31DEC1899', '2400', nvals, data1(2), 'CFS', 'PER-AVER', 0, status)
!  OK call zsrts (ifltab1, cpath, '30DEC1899', '2400', nvals, data1(2), 'CFS', 'PER-AVER', 0, status)
   call zsrts (ifltab1, cpath, '31DEC1899', '2400', nvals, data1(2), 'CFS', 'PER-AVER', 0, status)

   end subroutine

