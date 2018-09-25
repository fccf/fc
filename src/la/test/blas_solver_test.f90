program blas_solver_test
  use blas_interface
  use string, only: to_str
  implicit none


  real :: a(5,5),b(5)
  integer :: i

  a = 0.0
  do i = 1,5
    a(i,i) = 1.0
    b(i) = 1.0
  enddo

  write(unit=*, fmt=*) 'a = '//to_str(a)
  write(unit=*, fmt=*) 'b = '//to_str(b)

  call solve(a,b)

  write(unit=*, fmt=*) 'x = '//to_str(b)

end program blas_solver_test
