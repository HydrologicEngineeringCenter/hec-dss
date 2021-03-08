    subroutine JulianToDateTime(julian, scale, dateTime)
     
    integer :: julian  ! input julian
    integer :: scale   ! scale factor.  i.e. minutes per day (1440)
    character :: dateTime*(*) ! output
    
    integer min, nd, j
    character cd*20, ct*5

    j = julian / scale
    min = julian - (j * scale)
    call juldat(j, 1, cd, nd)
    call m2ihm(min, ct)
    dateTime = trim(cd(1:nd)) // ' ' // trim(ct)

     
    end 
    
  
! http://fortranwiki.org/fortran/show/newunit    
 ! This is a simple function to search for an available unit.
! LUN_MIN and LUN_MAX define the range of possible LUNs to check.
! The UNIT value is returned by the function, and also by the optional
! argument. This allows the function to be used directly in an OPEN
! statement, and optionally save the result in a local variable.
! If no units are available, -1 is returned.
subroutine newunit(unit)
  integer, intent(out) :: unit
! local
  integer, parameter :: LUN_MIN=10, LUN_MAX=1000
  logical :: opened
  integer :: lun
! begin
  unit=-1
  do lun=LUN_MIN,LUN_MAX
    inquire(unit=lun,opened=opened)
    if (.not. opened) then
     unit=lun
      exit
    end if
  end do
   
end subroutine newunit

    SUBROUTINE SaveToTextFile(filename,tag, times, values, numPoints, scale )
!
!    Function to write time series to a text file
            
      implicit none 
      character filename*(*)
      character tag*(*)
      character cdate*(30)
      integer times(1010), scale
      
      real values(1010)
      
      
      integer i,numPoints, u
      return
      call newunit(u)
      open(u,file =filename, status ='unknown', err = 100)
      write(u,*) '#',tag
      write(u,*) '#',numPoints
 
      do i=1, numPoints
        call JulianToDateTime(times(i),scale, cdate)
        write(u,*) cdate,' ,',times(i), values(i)
      end do 
      close(u)
       goto 200 
100   write(*,*)'error opening file ',filename    
200   return
      end