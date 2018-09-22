module time
  use iso_fortran_env, only: output_unit
  implicit none

  public :: timer, wtime, now, s2hms

  private
  !=============================================================================
  type step
    private
    character(:),allocatable :: name
    logical :: is_tracking = .FALSE.
    real    :: time    = 0.0
    real    :: persent = 0.0
    integer :: ncalls  = 0
    integer :: counter = 0

    type(step), pointer :: parent => null()
    type(step), pointer :: child  => null()
    ! type(step), pointer :: prev   => null()
    type(step), pointer :: next   => null()
  end type step

  !=============================================================================
  type timer
    private
    type(step), pointer :: root => null()
    type(step), pointer :: current => null()
  contains
    procedure :: start  => timer_start
    procedure :: stop   => timer_stop
    procedure :: report => timer_report
    procedure :: get_total => timer_get_total
  end type timer

contains
  !=============================================================================
  subroutine timer_start (this, name)
    !< start timer
    class(timer), intent(inout) :: this
    character(*), intent(in) :: name

    integer :: counter

    call system_clock (counter)

    if (.not.associated(this%root)) then
      allocate(this%root)
      this%current => this%root
    end if

    this%current => new_step(this%current%child)
    this%current%counter = counter
    this%current%is_tracking = .TRUE.

  contains
    recursive function new_step (current) result (new)
      type(step), pointer :: current, new
      if (.not.associated(current)) then
        allocate(current)
        current%name = name
        current%ncalls = 1
        current%parent => this%current
        new => current
      else if (current%name == name) then
        current%ncalls = current%ncalls + 1
        new => current
      else
        new => new_step(current%next)
      end if
    end function new_step

  end subroutine timer_start
  !=============================================================================
  subroutine timer_stop (this, name)
    !< stop timer
    class(timer), intent(inout) :: this
    character(*), intent(in), optional :: name

    integer :: counter, count_rate
    real    :: time

    if (present(name) .and. name /= this%current%name) then
      error stop 'current timer is ' // this%current%name // '; cannot stop ' // name
    end if

    call system_clock (counter, count_rate)

    time = (counter-this%current%counter)/real(count_rate)
    this%current%time = this%current%time + time
    this%current%is_tracking = .FALSE.

    this%current => this%current%parent

    if(.not.associated(this%current%parent)) then
      this%current%time = this%current%time + time
    endif

  end subroutine timer_stop
  !=============================================================================
  pure function timer_get_total(this) result(tot)
    class(timer), intent(in) :: this
    real :: tot
    tot = this%root%time
  end function timer_get_total
  !=============================================================================
  subroutine timer_report (this, unit, fmt)
    !< report time status information
    class(timer), intent(in) :: this
    integer, intent(in), optional :: unit
    character(*), intent(in), optional :: fmt

    type(step), pointer :: child

    character(:), allocatable :: lfmt, sep
    integer :: lunit

    lunit = output_unit
    if(present(unit)) lunit = unit

    lfmt = 'csv'
    if(present(fmt)) lfmt = fmt

    if(lfmt == 'csv') then
      sep = ','
    elseif(lfmt == 'tbl') then
      sep = '|'
    else
      error stop "unknow format for time report"
    endif

    if(lfmt == 'csv') then
      write(lunit,fmt='( a33, a17, a9, a17, a8)') &
      & "step ,", " time,", " ncalls,", " average,", ' persent '
    elseif(lfmt == 'tbl') then
      write(lunit,fmt='( a, a31, a, a16, a, a8, a, a16, a, a8, a)') &
      & sep, "step", sep,  " time,", sep, " ncalls,", sep, " average,", sep, ' persent ', sep
    endif

    if(lfmt == 'tbl') write(lunit,fmt='(a)')&
       & '|:------------------------------|:---------------|:------ |:-------------- |:-------|'

    child => this%root%child
    do while (associated(child))
      call write_step (child, '')
      child => child%next
    end do

    write(lunit,fmt='(a32, a,f16.1,a,a8,a, f16.1,a,a8)') &
    &          'total', sep,this%root%time, sep, '1', sep,this%root%time, sep, '100.0%'

  contains
    recursive subroutine write_step (me, pre)
      type(step), intent(inout) :: me
      character(*), intent(in)  :: pre
      type(step), pointer :: child

      character(:),allocatable :: pref

      me%persent = me%time/me%parent%time*100
      write(lunit,fmt='(a32, a,f16.1,a, i8, a, f16.1,a, f5.1, a)') &
          & pre//me%name, sep, me%time,sep,me%ncalls,sep,me%time/me%ncalls,sep,me%persent,'%'

      child => me%child
      do while (associated(child))
        pref = child%parent%name//'-'//pre
        call write_step (child, pref)
        child => child%next
      end do

    end subroutine write_step

  end subroutine timer_report
  !=============================================================================
  real function wtime()
     logical, save :: started=.false.
     integer, save :: counter0, counter1, counter_rate
     real  :: secs

     wtime = -1.0

     if(.not.started) then
        call system_clock(counter0)
        wtime = 0.0
        started=.true.
     else
       call system_clock (counter1, counter_rate)
       secs=  real(counter1-counter0)/real(counter_rate)
       wtime=secs
     endif

  end function wtime
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
  !=============================================================================
  function s2hms(sec) result(hms)
    !< return seconds to h:m:s string
    real, intent(in) :: sec
    character(:), allocatable :: hms
    integer :: hi,mi,si
    character(3) :: hs,ms,ss

    si = nint(sec)
    hi = int(si/3600)
    si = si - 3600*hi
    mi = int(si/60)
    si = si - 60*mi

    write(hs,'(i0)') hi
    write(ms,'(i0)') mi
    write(ss,'(i0)') si
    hms = trim(adjustl(hs))//'h:'//trim(adjustl(ms))//'m:'//trim(adjustl(ss))//'s'

  end function s2hms

end module time
