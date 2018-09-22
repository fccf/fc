program time_test
  use time
  use string
  use iso_c_binding
  implicit none

  real :: t1

  print*, '# time_date '
  print*, now()
  print*,s2hms(36009.7)

  t1 = wtime()

  call sleep(1)
  print*, '# wtime '
  print*, to_str(wtime())

  call timer_test

contains

  subroutine timer_test
    use time
    implicit none

    type(timer) :: tt

    call tt%start('A')
    call sleep(1)
    call tt%stop('A')

    call tt%start('A')
    call sleep(1)
    call tt%stop('A')

    call tt%start('B')
    call sleep(1)
    call tt%stop('B')

    call tt%start('C')
    call sleep(1)
    call tt%start('D')
    call sleep(1)
    call tt%stop('D')
    call tt%stop('C')

    print*, '# csv timer report'
    call tt%report(unit = 6, fmt = 'csv')

    print*, '# table timer report'
    call tt%report(unit = 6, fmt = 'tbl')

  end subroutine timer_test

end program time_test
