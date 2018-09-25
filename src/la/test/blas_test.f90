program blas_test
  use blas_interface
  use string, only:to_str
  implicit none

  real :: a(5,5),b(5,5),c(5,5)
  real, allocatable :: ai(:,:), ao(:,:)
  integer :: i
  real :: v(size(A,1)),em(size(A,1),size(A,1))

  a = reshape([(i,i=1,25)],[5,5])
  b = 0.0
  do i = 1,5
    b(i,i) = 1.0
  enddo

  c = mult(a,b)
  print*, 'a = '//to_str(a)
  print*, 'b = '//to_str(b)
  print*, 'c = '//to_str(c)

  call evds(a, em, v)
  call rvd(a,ai,ao)

end program blas_test
