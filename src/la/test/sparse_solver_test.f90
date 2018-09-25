program sparse_solver_test
  use sparse_solver
  use sparse_matrix
  use string, only: to_str
  implicit none

  type(iter_solver) :: gmres

  type(csr_matrix) :: a_csr
  real :: a(5,5),b(5),x(5)
  integer :: i
  a = 0.0
  do i = 1,5
    a(i,i) = 1.0
    b(i) = 1.0
  enddo

  call a_csr%set(a)
  call gmres%set(itmax=1000,irst=100,eps = 1.0e-5,itrace = 1)
  call gmres%solve(a_csr,b,x)

  call gmres%print_summary(6)
  write(unit=*, fmt=*) 'a = '//to_str(a)
  write(unit=*, fmt=*) 'b = '//to_str(b)
  write(unit=*, fmt=*) 'x = '//to_str(x)


end program sparse_solver_test
