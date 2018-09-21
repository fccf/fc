module time

  implicit none

  public :: now
  
  private

contains
  !=============================================================================
  function now()
    !< return time now as a string
    character(:), allocatable :: now

    integer :: y,m,d,h,n,s,values(8)
    character(len=10) :: date
    character(len=8)  :: time

    call date_and_time ( values = values )

    y = values(1)
    m = values(2)
    d = values(3)
    h = values(5)
    n = values(6)
    s = values(7)

    write(date,'(i0,a,i0,a,i0)') y,'-',m,'-',d
    write(time,'(i0,a,i0,a,i0)') h,':',n,':',s

    now = trim(adjustl(date))//' '//trim(adjustl(time))

  end function now

end module time
